;;;; This file implements the SSA conversion phase in the compiler. We
;;;; compute reaching definitions for all eligible set variables, and
;;;; then we try to rewrite sets away by converting them to let and
;;;; assignment lambda bindings of new variables which are never set.
;;;;
;;;; The basic algorithm and analysis follow Braun et al., "Simple and
;;;; Efficient Construction of Static Single Assignment Form"
;;;; (2013). Essentially, reaching definitions are found by walking
;;;; block predecessors on demand starting backward from variable
;;;; references. The main desirable property of this algorithm for IR1
;;;; purposes is that no dominance frontier computations are needed,
;;;; making it easy to incrementally convert individual variables to
;;;; SSA as part of local call conversion without the need for any DFO
;;;; computation.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;; A list of phis created when computing the reaching definitions for
;;; a some variable.
(defvar *created-phis*)


;;; Resolve any forwarded definitions for DEF via path compression.
(defun resolve-definition (def)
  (if (phi-p def)
      (let ((rep (phi-replacement def)))
        (if rep
            (let ((actual (resolve-definition rep)))
              (setf (phi-replacement def) actual)
              actual)
            def))
      def))

;;; Return the block local definition for VAR walking backwards from
;;; NODE. If there is none, return NIL.
(defun find-block-local-definition (var node)
  (declare (type lambda-var var)
           (type node node))
  (let ((home-bind (lambda-bind (lambda-var-home var))))
    (do* ((curr node (ctran-use ctran))
          (ctran (node-prev curr) (node-prev curr)))
         ((null curr) nil)
      (cond ((eq curr home-bind)
             (return var))
            ((and (set-p curr) (eq (set-var curr) var))
             (return curr))
            ((eq (ctran-kind ctran) :block-start)
             (return nil))))))

;;; True if VAR is set within the extent of CLEANUP.
(defun set-in-cleanup-p (var cleanup)
  (dolist (set (lambda-var-sets var))
    (do-nested-cleanups (enclosing set)
      (when (eq enclosing cleanup)
        (return-from set-in-cleanup-p t)))))

;;; Find the definition of VAR reaching the end of BLOCK, a
;;; predecessor of a block in HOME.
;;;
;;; If BLOCK and HOME share a home lambda, this is just the definition
;;; reaching BLOCK's last node.
;;;
;;; If they differ, this is a non-local exit, which might trigger
;;; anywhere within the dynamic extent of its entry. If VAR is set
;;; within the extent, the definition is statically unknown and can't
;;; be converted. Otherwise, as VAR isn't closed over and so isn't set
;;; by the code doing the exit, the definition is the one that reaches
;;; the entry node of the extent.
(defun find-block-end-definition (var block home)
  (declare (type lambda-var var)
           (type cblock block))
  (let ((last (block-last block)))
    (if (eq (block-home-lambda block) home)
        (find-node-definition var last)
        (let* ((entry (exit-entry last))
               (cleanup (entry-cleanup entry)))
          (aver (eq (node-home-lambda entry) home))
          (when (set-in-cleanup-p var cleanup)
            (throw 'ineligible nil))
          (find-node-definition var entry)))))

;;; Find the definition of VAR reaching the start of BLOCK by
;;; recursively looking for definitions in the block predecessors. If
;;; there are multiple definitions reaching this block, we record the
;;; value-join with a PHI structure. In addition, we break any cycles
;;; in the walk by keeping track of block flags of the following
;;; states:
;;;
;;; 1. VAR if the block has already been visited once.
;;; 2. A phi for VAR if the block has already allocated the phi.
;;; 3. The unique reaching definition tagged by var if there is one.
;;; 4. Anything else if the block has not been visited yet.
;;;
;;; This scheme avoids having to clear block flags for distinct
;;; variable conversion.
;;;
;;; When BLOCK has no predecessors and is thus unreachable, we return
;;; NIL.
(defun find-block-start-definition (var block)
  (declare (type lambda-var var)
           (type cblock block))
  (let* ((preds (block-pred block))
         (flag (block-flag block))
         (home (lambda-home (lambda-var-home var))))
    (aver (eq (block-home-lambda block) home))
    (cond ((null preds)
           nil)
          ((walked-start-definition var block))
          ((eq flag var)
           (let ((phi (make-phi var block)))
             (setf (block-flag block) phi)
             phi))
          (t
           (setf (block-flag block) var)
           (collect ((operands))
             (let ((definition nil)
                   (same t))
               (dolist (pred preds)
                 (let ((pred-definition (find-block-end-definition var pred home)))
                   (operands pred-definition)
                   (cond ((null pred-definition))
                         ((null definition)
                          (setq definition pred-definition))
                         ((neq pred-definition definition)
                          (setq same nil)))))
               (let ((new-flag (block-flag block)))
                 (cond ((and (phi-p new-flag)
                             (eq (phi-var new-flag) var))
                        (setf (phi-operands new-flag) (operands))
                        (push new-flag *created-phis*)
                        new-flag)
                       (same
                        (setf (block-flag block) (cons var definition))
                        definition)
                       (t
                        (let ((phi (make-phi var block)))
                          (push phi *created-phis*)
                          (setf (phi-operands phi) (operands))
                          (setf (block-flag block) phi)
                          phi))))))))))

;;; Return the definition of VAR reaching the start of BLOCK.
(defun walked-start-definition (var block)
  (let ((flag (block-flag block)))
    (cond ((and (phi-p flag) (eq (phi-var flag) var))
           flag)
          ((and (consp flag) (eq (car flag) var))
           (cdr flag)))))

;;; Find the definition of VAR reaching NODE.
(defun find-node-definition (var node)
  (declare (type lambda-var var)
           (type node node))
  (or (find-block-local-definition var node)
      (find-block-start-definition var (node-block node))))

;;; Return the definition of VAR known to reach NODE without making any
;;; phis, as a variable nothing reads can't have them, or NIL.
(defun find-known-definition (var node)
  (let* ((block (node-block node))
         (pred (first (block-pred block))))
    (or (find-block-local-definition var node)
        (walked-start-definition var block)
        (when (and (singleton-p (block-pred block))
                   (block-last pred)
                   (neq (block-flag block) var))
          (setf (block-flag block) var)
          (find-known-definition var (block-last pred))))))

;;; Return an alist from the targets of the non-local exits to the entries
;;; of VAR's home lambda to the definitions known to hold there.
(defun find-nlx-definitions (var)
  (let ((home (lambda-home (lambda-var-home var)))
        (definitions '()))
    (dolist (entry (lambda-entries home) definitions)
      (dolist (exit (entry-exits entry))
        (let* ((block (node-block exit))
               (target (first (block-succ block))))
          (when (and (neq (block-home-lambda block) home)
                     (not (set-in-cleanup-p var (entry-cleanup entry))))
            (let ((definition (find-known-definition var entry)))
              (when definition
                (push (cons target (resolve-definition definition))
                      definitions)))))))))

;;; Eliminate any superfluous phis in PHIS by finding strongly
;;; connected components with Tarjan's SCC algorithm and setting
;;; definition forwarding pointers appropriately. In particular, an
;;; SCC where there is only one reaching definition from outside of
;;; the component is collapsed by forwarding each phi in the component
;;; to that definition. An SCC which has no reaching definitions from
;;; outside the SCC comes from dead code, which keeps referring to the
;;; variable, and the NILs of dead predecessors can be ignored. An SCC
;;; with several is not redundant, but the phis in it with no operands
;;; from outside it may still be, so we recursively traverse the inner
;;; SCCs (Braun et al. 2013, algorithm 5), making sure to mark its
;;; members unvisited first so the walk bottoms out.
(defun eliminate-redundant-phis-scc (phis)
  (let ((index-counter 0)
        (stack nil))
    (labels
        ((strong-connect (phi)
           (setf (phi-index phi) index-counter
                 (phi-lowlink phi) index-counter
                 (phi-on-stack-p phi) t)
           (incf index-counter)
           (push phi stack)

           (dolist (raw-op (phi-operands phi))
             (let ((op (resolve-definition raw-op)))
               (when (phi-p op)
                 (cond
                   ((= (phi-index op) -1)
                    (strong-connect op)
                    (setf (phi-lowlink phi)
                          (min (phi-lowlink phi) (phi-lowlink op))))
                   ((phi-on-stack-p op)
                    (setf (phi-lowlink phi)
                          (min (phi-lowlink phi) (phi-index op))))))))

           (when (= (phi-lowlink phi) (phi-index phi))
             (let ((scc nil))
               (loop
                 (let ((node (pop stack)))
                   (setf (phi-on-stack-p node) nil)
                   (push node scc)
                   (when (eq node phi) (return))))
               (evaluate-scc scc))))

         (evaluate-scc (scc)
           (let ((outer-operands nil))
             (dolist (node scc)
               (dolist (operand (phi-operands node))
                 (let ((resolved (resolve-definition operand)))
                   (unless (or (null resolved)
                               (member resolved scc))
                     (pushnew resolved outer-operands)))))
             (cond ((null outer-operands)
                    (dolist (node scc)
                      (setf (phi-replacement node) (phi-var node))))
                   ((null (rest outer-operands))
                    (let ((outer-operand (first outer-operands)))
                      (dolist (node scc)
                        (setf (phi-replacement node) outer-operand))))
                   (t
                    (let ((inner (remove-if-not
                                  (lambda (node)
                                    (every (lambda (operand)
                                             (let ((resolved (resolve-definition operand)))
                                               (or (null resolved)
                                                   (member resolved scc))))
                                           (phi-operands node)))
                                  scc)))
                      (when inner
                        (dolist (node inner)
                          (setf (phi-index node) -1))
                        (eliminate-redundant-phis-scc inner))))))))
      (dolist (phi phis)
        (when (and (null (phi-replacement phi))
                   (= (phi-index phi) -1))
          (strong-connect phi))))))

;;; Compute reaching definitions for VAR, annotating each of VAR's
;;; references with its reaching definition. A definition may be:
;;; -- VAR itself, representing the initial variable binding.
;;; -- A set node representing the value after the assignment.
;;; -- A phi representing a value-join.
;;; -- NIL, when the reference itself is unreachable.
;;;
;;; We do this by walking the graph backward from each of VAR's
;;; references. Since this process creates many phis which may be
;;; trivial or redundant, we do a post-pass using Tarjan's SCC
;;; algorithm to eliminate them. (See section 3.2 of Braun et
;;; al. 2013). The set of phis which survive is returned as a list.
;;;
;;; If a phi survives which a non-local exit reaches, we have to give
;;; up on VAR, as the phi can't be implemented by an assignment lambda
;;; in this case.
;;;
;;; When DEBUG-P is true, we insert extra information and walk back
;;; from every block in the scope of VAR for the debugger.
(defun compute-reaching-definitions (var debug-p)
  (declare (type lambda-var var))
  (let ((*created-phis* '())
        (home (lambda-home (lambda-var-home var)))
        (previous '()))
    (dolist (ref (leaf-refs var))
      (setf (ref-ssa-definition ref)
            (find-node-definition var ref)))
    (when debug-p
      ;; What each set replaces, which the debugger keeps until then.
      (dolist (set (lambda-var-sets var))
        (push (cons set (let ((prev (ctran-use (node-prev set))))
                          (if prev
                              (find-node-definition var prev)
                              (find-block-start-definition var (node-block set)))))
              previous)))
    (when debug-p
      (let* ((bind-block (node-block (lambda-bind (lambda-var-home var))))
             (component (block-component bind-block)))
        (labels ((lexically-bound-p (lexenv)
                   (let ((lambda (lexenv-lambda lexenv)))
                     (or (find var (lexenv-vars lexenv) :key #'cdr :test #'eq)
                         (and lambda (lambda-call-lexenv lambda)
                              (lexically-bound-p (lambda-call-lexenv lambda))))))
                 (in-scope-p (block)
                   (lexically-bound-p (node-lexenv (block-start-node block)))))
          (do-blocks (block component)
            (when (and (eq (block-home-lambda-or-null block) home)
                       (neq block bind-block)
                       (in-scope-p block))
              (find-block-start-definition var block))))))

    (eliminate-redundant-phis-scc *created-phis*)

    (dolist (ref (leaf-refs var))
      (setf (ref-ssa-definition ref)
            (resolve-definition (ref-ssa-definition ref))))
    (dolist (entry previous)
      (setf (cdr entry) (resolve-definition (cdr entry))))

    (collect ((phis))
      (dolist (phi *created-phis*)
        (unless (phi-replacement phi)
          (when (some (lambda (pred)
                        (neq (block-home-lambda pred) home))
                      (block-pred (phi-block phi)))
            (throw 'ineligible nil))
          (phis phi)
          (setf (phi-operands phi)
                (mapcar #'resolve-definition (phi-operands phi)))))
      (values (phis) previous))))

;;; Returns the SSA variable representing the value of this definition
;;; of VAR once all definitions have been SSA converted.
(defun definition-ssa-var (definition var)
  (etypecase definition
    (null var)
    (lambda-var definition)
    (cset (set-var definition))
    (phi (phi-new-var definition))))

(defun ssa-name (var)
  (gensym (concatenate 'string "SSA" (string (leaf-%source-name var)))))

;;; Convert SET into a fresh let binding and remove it from the flow
;;; graph. The new variable is left as a cookie in SET's VAR
;;; slot. Additionally, when the set is in value position, its lvar's
;;; destination is wired up to reference the new variable instead.
(defun ssa-convert-set (set var)
  (declare (type cset set)
           (type lambda-var var))
  (when (node-prev set)
    (let ((home (lambda-home (lambda-var-home var)))
          (value (set-value set))
          (result-lvar (node-lvar set)))
      (with-ir1-environment-from-node set
        (let* ((bind (make-bind))
               (new-var (make-lambda-var (ssa-name var) :origin var))
               (fun (make-clambda :vars (list new-var)
                                  :kind (functional-kind-attributes let)
                                  :bind bind
                                  :home home
                                  :%debug-name `(set ,(leaf-%source-name var))))
               (fun-ref (make-ref fun)))
          (setf (lambda-var-home new-var) fun)
          (setf (leaf-ever-used new-var) t)
          (setf (bind-lambda bind) fun)
          (setf (lambda-tail-set fun) (make-tail-set (list fun)))
          (setf (lambda-environment fun) (lambda-environment home))
          (push fun (lambda-lets home))
          (push fun-ref (leaf-refs fun))
          (insert-node-after set bind)
          (let* ((fun-lvar (make-lvar))
                 (call (make-combination fun-lvar)))
            (use-lvar fun-ref fun-lvar)
            (setf (lvar-dest fun-lvar) call)
            (insert-node-before bind call)
            (setf (combination-kind call) :local
                  (combination-args call) (list value))
            (setf (lvar-dest value) call)
            (insert-node-before call fun-ref))
          (when result-lvar
            (let ((result-ref (make-ref new-var)))
              (push result-ref (leaf-refs new-var))
              (insert-node-after bind result-ref)
              (use-lvar result-ref result-lvar)
              (%delete-lvar-use set)))
          (setf (node-lvar set) nil)
          (unlink-node set)
          (setf (set-var set) new-var))))))

;;; Ensure BLOCK binds NEW-VAR with an assignment lambda. If an
;;; assignment lambda already exists, just add the variable to the
;;; lambda.
(defun ssa-ensure-join-lambda (block new-var)
  (declare (type cblock block)
           (type lambda-var new-var))
  (aver (rest (block-pred block)))
  (let* ((start-node (block-start-node block))
         (home (block-home-lambda block))
         (fun (if (and (bind-p start-node)
                       (let ((fun (bind-lambda start-node)))
                         ;; LETs cannot head join points.
                         (aver (not (functional-kind-eq fun let)))
                         (functional-kind-eq fun assignment)))
                  (bind-lambda start-node)
                  (with-ir1-environment-from-node start-node
                    (let* ((bind (make-bind))
                           (fun (make-clambda :kind (functional-kind-attributes assignment)
                                              :bind bind
                                              :home home
                                              :%debug-name "phi")))
                      (setf (bind-lambda bind) fun)
                      (setf (lambda-tail-set fun) (make-tail-set (list fun)))
                      (setf (lambda-call-lexenv fun) (node-lexenv start-node))
                      (setf (lambda-environment fun) (lambda-environment home))
                      (push fun (lambda-lets home))
                      (insert-node-before start-node bind)
                      fun))) ))
    (aver (not (memq new-var (lambda-vars fun))))
    (setf (lambda-vars fun) (nconc (lambda-vars fun) (list new-var)))
    (setf (lambda-var-home new-var) fun)))

;;; Ensure PRED local calls the assignment lambda FUN passing the
;;; reference to VAR at argument N, splitting the edge between PRED
;;; and BLOCK if needed. This is the only place in SSA conversion
;;; which may modify the block structure of the flow graph.
(defun ssa-ensure-pred-local-calls (pred block fun var n)
  (declare (type cblock block)
           (type clambda fun)
           (type lambda-var var))
  (let ((last (block-last pred)))
    (with-ir1-environment-from-node last
      (let ((arg-ref (make-ref var))
            (arg-lvar (make-lvar)))
        (push arg-ref (leaf-refs var))
        (use-lvar arg-ref arg-lvar)
        (cond ((and (combination-p last)
                    (eq (combination-kind last) :local)
                    (eq (combination-lambda last) fun))
               (aver (= (length (combination-args last)) n))
               (insert-node-before last arg-ref)
               (setf (lvar-dest arg-lvar) last)
               (setf (combination-args last)
                     (nconc (combination-args last) (list arg-lvar))))
              (t
               (let* ((ctran (make-ctran :block-start))
                      (edge (make-block-key :start ctran :pred '() :succ '()))
                      (fun-ref (make-ref fun))
                      (fun-lvar (make-lvar))
                      (call (make-combination fun-lvar)))
                 (setf (ctran-block ctran) edge)
                 (link-node-to-previous-ctran call ctran)
                 (setf (block-last edge) call)
                 (insert-node-before call arg-ref)
                 (setf (lvar-dest arg-lvar) call)
                 (insert-node-before arg-ref fun-ref)
                 (push fun-ref (leaf-refs fun))
                 (use-lvar fun-ref fun-lvar)
                 (setf (lvar-dest fun-lvar) call)
                 (setf (combination-kind call) :local
                       (combination-args call) (list arg-lvar))
                 (change-block-successor pred block edge)
                 (add-to-dfo edge (block-prev block))
                 (link-blocks edge block))))))))

;;; Tell the debugger where VAR, now converted, has which version: from the
;;; bindings of its versions and the targets of non-local exits on, and from a
;;; set with no version, whose value nothing holds.
(defun note-debug-bindings (var sets phis nlx-definitions)
  (flet ((note (version ctran)
           (setf (ctran-debug-bindings ctran)
                 (nconc (ctran-debug-bindings ctran) (list (cons var version)))))
         (bind-point (version)
           (node-prev (lambda-bind (lambda-var-home version)))))
    (note var (bind-point var))
    (dolist (set sets)
      (cond ((neq (set-var set) var)
             (note (set-var set) (bind-point (set-var set))))
            ((node-prev set)
             (note nil (node-prev set)))))
    (dolist (phi phis)
      (note (phi-new-var phi) (bind-point (phi-new-var phi))))
    (loop for (target . definition) in nlx-definitions
          for version = (definition-ssa-var definition var)
          ;; A set which is deleted has no version.
          unless (or (null definition)
                     (and (set-p definition) (eq version var)))
          do (note version (block-start target)))))

;;; Convert VAR to SSA form. We first compute reaching definitions for
;;; VAR, and then convert each of VAR's sets and phis to fresh
;;; variable bindings. When a set never actually reaches any reference
;;; or phi, it can be completely removed. The value join semantics of
;;; a phi is implemented via local calls to an assignment lambda
;;; binding the phi's variable. We fix up the local call semantics
;;; after assignment lambdas are introduced for all phis, once the new
;;; variable bindings have been introduced for all definitions.
;;;
;;; VAR's references are then finally changed to reference the new SSA
;;; "versions" of VAR.
(defun ssa-convert-var (var)
  (catch 'ineligible
    (let* ((sets (lambda-var-sets var))
           (debug-p (or (debug-environment-var-p var)
                        (preserve-set-only-debug-var-p var)))
           (reaching (multiple-value-list (compute-reaching-definitions var debug-p)))
           (phis (first reaching))
           ;; Before conversion takes the sets out of the flow graph.
           (nlx-definitions (find-nlx-definitions var))
           (refs (lambda-var-refs var)))

      (setf (lambda-var-origin var) var)

      (dolist (set sets)
        (map-all-uses (lambda (node)
                        (when (combination-p node)
                          (setf (combination-info node) :important-result-discarded)))
                      (set-value set)
                      :sets t))

      (dolist (ref refs)
        (let ((definition (ref-ssa-definition ref)))
          (when (set-p definition)
            (ssa-convert-set definition var))))

      (dolist (phi phis)
        (aver (rest (phi-operands phi)))
        (dolist (operand (phi-operands phi))
          (when (set-p operand)
            (ssa-convert-set operand var)))
        (let ((block (phi-block phi))
              (new-var (make-lambda-var (ssa-name var) :origin var)))
          (aver (= (length (block-pred block))
                   (length (phi-operands phi))))
          (setf (leaf-ever-used new-var) t)
          (setf (phi-new-var phi) new-var)
          (ssa-ensure-join-lambda block new-var)))

      (dolist (set sets)
        (when (or (node-lvar set) debug-p)
          (ssa-convert-set set var)))

      (dolist (phi phis)
        (let* ((block (phi-block phi))
               (fun (bind-lambda (block-start-node block)))
               (new-var (phi-new-var phi))
               (n (position new-var (lambda-vars fun))))
          (loop for operand in (phi-operands phi)
                for pred in (block-pred block)
                do (ssa-ensure-pred-local-calls pred block fun
                                                (definition-ssa-var operand var)
                                                n))))

      (loop for (set . definition) in (second reaching)
            for version = (set-var set)
            for prev = (and definition (definition-ssa-var definition var))
            when (and (neq version var) (lambda-var-p prev))
              do (setf (lambda-var-debug-previous version) prev))

      (when debug-p
        (setf (lambda-var-kept-for-debug var) t)
        (dolist (set sets)
          (setf (lambda-var-kept-for-debug (set-var set)) t))
        (dolist (phi phis)
          (setf (lambda-var-kept-for-debug (phi-new-var phi)) t)))

      (note-debug-bindings var sets phis nlx-definitions)

      (dolist (set sets)
        (when (eq (set-var set) var)
          (flush-dest (set-value set))
          (unlink-node set)))

      (setf (lambda-var-sets var) '())

      (dolist (ref refs)
        (change-ref-leaf ref (definition-ssa-var (ref-ssa-definition ref) var)))

      t)))

;;; Could VAR ever be converted? Variables which are special or
;;; already determined to be indirect cannot be converted, as they are
;;; conceptually like memory references. Even if it is closed over, it
;;; may not be after future optimizations, so we leave the possibility
;;; for conversion open.
(defun ssa-possible-candidate-p (var)
  (and (lambda-var-sets var)
       (leaf-refs var)
       (not (lambda-var-specvar var))
       (not (lambda-var-indirect var))
       (let ((home (lambda-var-home var)))
         (and home
              (not (functional-kind-eq home deleted zombie))
              (lambda-bind home)))))

;;; Can VAR currently be converted?
(defun ssa-convertible-var-p (var)
  (and (ssa-possible-candidate-p var)
       (not (closure-var-p var))))

;;; Note that VAR should be reconsidered for SSA conversion. This
;;; usually happens when a reference which was keeping it closed over
;;; has been deleted. We note rather than convert on the spot as it is
;;; dangerous to do SSA conversion in the middle of IR1 optimizations,
;;; as conversion can hack the flow graph or create new lambdas.
(defun note-ssa-candidate (var)
  (declare (type lambda-var var))
  (when (ssa-possible-candidate-p var)
    (pushnew var (lambda-ssa-pending (lambda-home (lambda-var-home var)))))
  (values))

;;; Note that conversion changed COMPONENT.
(defun note-ssa-converted (component)
  (setf (component-reanalyze component) t)
  (reoptimize-component component t)
  (values))

;;; Act on whatever was requested since the pass last ran.
(defun ssa-convert-pending (component)
  (declare (type component component))
  (let ((walked nil)
        (converted nil))
    (dolist (fun (component-lambdas component))
      (let ((pending (lambda-ssa-pending fun)))
        (setf (lambda-ssa-pending fun) nil)
        (dolist (var pending)
          (cond ((ssa-convertible-var-p var)
                 (setq walked t)
                 ;; Giving up leaves unfinished phis in the block flags, and
                 ;; VAR may be pending in another lambda as well.
                 (if (ssa-convert-var var)
                     (setq converted t)
                     (clear-flags component)))
                ((ssa-possible-candidate-p var)
                 (push var (lambda-ssa-pending fun)))))))
    (when converted
      (note-ssa-converted component))
    (when walked
      (clear-flags component)))
  (values))

;;; Convert COMPONENT to SSA form.
(defun ssa-convert-component (component)
  (declare (type component component))
  (dolist (fun (component-lambdas component))
    (dolist (fun (cons fun (lambda-lets fun)))
      (mapc #'note-ssa-candidate (lambda-vars fun))))
  (ssa-convert-pending component))
