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
          (dolist (set (lambda-var-sets var))
            (do-nested-cleanups (enclosing set)
              (when (eq enclosing cleanup)
                (throw 'ineligible nil))))
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
;;; When BLOCK has no predecessors, it is dead and will be removed
;;; later. No definition reaches it then, and we return NIL. Only the
;;; definitions which do reach a block count towards whether they are
;;; all the same, so that dead predecessors don't make phis look
;;; needed.
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

;;; Return the definition of VAR reaching the start of BLOCK, if the
;;; walks for its reaching definitions went through BLOCK.
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

;;; Eliminate any superfluous phis in PHIS by finding strongly
;;; connected components with Tarjan's SCC algorithm and setting
;;; definition forwarding pointers appropriately. In particular, an
;;; SCC where there is only one reaching definition from outside of
;;; the component is collapsed by forwarding each phi in the component
;;; to that definition. An SCC which has no reaching definitions from
;;; outside the SCC comes from dead code and can be ignored, as can
;;; the NILs of dead predecessors. An SCC with several is not
;;; redundant, but the phis in it with no operands from outside it may
;;; still be, so we recursively traverse the inner SCCs (Braun et
;;; al. 2013, algorithm 5), making sure to mark its members unvisited
;;; first so the walk bottoms out.
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
             (cond ((null outer-operands))
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
;;; -- NIL, when nothing reaches the reference because it is dead.
;;;
;;; We do this by walking the graph backward from each of VAR's
;;; references. Since this process creates many phis which may be
;;; trivial or redundant, we do a post-pass using Tarjan's SCC
;;; algorithm to eliminate them. (See section 3.2 of Braun et
;;; al. 2013). In addition, single operand phis in single predecessor
;;; blocks are also eliminated here. The set of phis which survive is
;;; returned as a list.
;;;
;;; If a phi survives which a non-local exit reaches, we have to give
;;; up on VAR, as the phi can't be implemented by an assignment lambda
;;; in this case.
;;;
;;; When the debugger uses VAR, we also walk backward from the start
;;; of every block VAR is in scope in, which gives each of them the
;;; definition reaching it, phis included.
(defun compute-reaching-definitions (var debugger-uses-p)
  (declare (type lambda-var var))
  (let ((*created-phis* '())
        (home (lambda-home (lambda-var-home var))))
    (dolist (ref (leaf-refs var))
      (setf (ref-ssa-definition ref)
            (find-node-definition var ref)))
    (when debugger-uses-p
      (let ((home-bind (lambda-bind (lambda-var-home var))))
        (do-blocks (block (block-component (node-block home-bind)))
          (let ((start (block-start-node block)))
            (when (and (eq (block-home-lambda-or-null block) home)
                       (neq start home-bind)
                       (lexenv-bound-var (node-lexenv start) var))
              (find-block-start-definition var block))))))

    (eliminate-redundant-phis-scc *created-phis*)

    (dolist (phi *created-phis*)
      (when (null (rest (phi-operands phi)))
        (setf (phi-replacement phi) (first (phi-operands phi)))))

    (dolist (ref (leaf-refs var))
      (setf (ref-ssa-definition ref)
            (resolve-definition (ref-ssa-definition ref))))

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
      (phis))))

;;; Returns the SSA variable representing the value of this definition
;;; of VAR once all definitions have been SSA converted. Dead code,
;;; where no definition reaches, keeps referring to VAR.
(defun definition-ssa-var (definition var)
  (etypecase definition
    (null var)
    (lambda-var definition)
    (cset (set-var definition))
    (phi (phi-new-var definition))))

;;; Return a variable to hold some of the values of VAR, which is set:
;;; that of a set of VAR, or of a phi joining several. The debugger
;;; shows it as VAR where the lexical environment binds VAR to it.
(defun make-ssa-var (var)
  (declare (type lambda-var var))
  (let ((new (make-lambda-var
              (gensymify (concatenate 'string "SSA-"
                                      (string (leaf-%source-name var)))))))
    (setf (lambda-var-origin new) var)
    new))

;;; Convert SET into a fresh let binding and remove it from the flow
;;; graph. The new variable is left as a cookie in SET's VAR
;;; slot. Additionally, when the set is in value position, its lvar's
;;; destination is wired up to reference the new variable instead.
(defun ssa-convert-set (set var)
  (declare (type cset set)
           (type lambda-var var))
  (when (node-prev set)
    (note-set-value-used set)
    (let ((home (lambda-home (lambda-var-home var)))
          (value (set-value set))
          (result-lvar (node-lvar set)))
      (with-ir1-environment-from-node set
        (let* ((bind (make-bind))
               (new-var (make-ssa-var var))
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
          ;; HOME may have been given its environment already.
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
          (setf (lambda-var-sets var) (delq1 set (lambda-var-sets var)))
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

;;; Return LEXENV, or a copy of it in which VERSION is the version of VAR
;;; bound, when VAR is in scope.
(defun lexenv-with-ssa-version (lexenv var version)
  (declare (type lexenv lexenv) (type lambda-var var version))
  (let* ((name (leaf-%source-name var))
         (vars (lexenv-vars lexenv))
         (tail (member-if (lambda (entry)
                            (and (eq (car entry) name)
                                 (lambda-var-p (cdr entry))
                                 (eq (debug-var-origin (cdr entry)) var)))
                          vars))
         (new-vars
           (if tail
               (unless (eq (cdar tail) version)
                 (append (ldiff vars tail)
                         (acons name version (cdr tail))))
               ;; In scope through the lexical environment of a call.
               (let ((outer (lexenv-bound-var lexenv var)))
                 (when (and outer (neq outer version))
                   (append vars (list (cons name version))))))))
    (if new-vars
        (let ((new (make-lexenv :default lexenv
                                :policy (lexenv-%policy lexenv))))
          (setf (lexenv-vars new) new-vars)
          new)
        lexenv)))

;;; Make the version of VAR holding its value at each node the binding
;;; of VAR in the lexical environment of the node, as far as the
;;; debugger is concerned, so that the debugger still knows where the
;;; value of VAR is once the bindings of versions are optimized away.
;;; The walks for the reaching definitions found the definition
;;; reaching the start of each block they went through; VAR is dead at
;;; the start of the other blocks, and has no version there, nor after
;;; a set whose value is never used, which is still there to be
;;; deleted.
(defun note-ssa-version-scopes (var)
  (declare (type lambda-var var))
  (let ((home (lambda-home (lambda-var-home var)))
        ;; Stands for the value of VAR where no version has it.
        (none (make-ssa-var var))
        ;; The lexical environments made, as (LEXENV VERSION . MADE),
        ;; and the one used last.
        (stamps '())
        (last nil))
    (flet ((stamp (lexenv version)
             (unless (and last (eq (car last) lexenv) (eq (cadr last) version))
               (setq last
                     (or (find-if (lambda (stamp)
                                    (and (eq (car stamp) lexenv)
                                         (eq (cadr stamp) version)))
                                  stamps)
                         (car (push (list* lexenv version
                                           (lexenv-with-ssa-version lexenv var version))
                                    stamps)))))
             (cddr last)))
      (do-blocks (block (block-component (node-block (lambda-bind home))))
        (when (eq (block-home-lambda-or-null block) home)
          (let ((version (let ((definition (walked-start-definition var block)))
                           (if definition
                               (definition-ssa-var (resolve-definition definition) var)
                               none))))
            (do-nodes (node nil block)
              (cond ((bind-p node)
                     (setq version (or (find var (lambda-vars (bind-lambda node))
                                             :key #'debug-var-origin)
                                       version)))
                    ((and (set-p node) (eq (set-var node) var))
                     (setq version none)))
              (setf (node-lexenv node) (stamp (node-lexenv node) version))))))))
  (values))

;;; Insert a use of VAR right after its binding which only keeps its
;;; value live, for the debugger.
(defun insert-keep-live (var)
  (declare (type lambda-var var))
  (let ((bind (lambda-bind (lambda-var-home var))))
    (with-ir1-environment-from-node bind
      (let* ((fun-lvar (make-lvar))
             (call (make-combination fun-lvar)))
        (setf (lvar-dest fun-lvar) call)
        (insert-node-after bind call)
        (insert-ref-before (find-free-fun '%keep-live "SSA conversion") call fun-lvar)
        (setf (combination-args call) (list (insert-ref-before var call))
              (combination-kind call) :known
              (combination-fun-info call) (info :function :info '%keep-live))))))

;;; Convert VAR to SSA form. We first compute reaching definitions for
;;; VAR, and then convert each of VAR's sets and phis to fresh
;;; variable bindings. When a set never actually reaches any reference
;;; or phi, it can be completely removed, once the debugger has been
;;; told where VAR has which version. The value join semantics of
;;; a phi is implemented via local calls to an assignment lambda
;;; binding the phi's variable. We fix up the local call semantics
;;; after assignment lambdas are introduced for all phis, once the new
;;; variable bindings have been introduced for all definitions.
;;;
;;; VAR's references are then finally changed to reference the new SSA
;;; "versions" of VAR.
;;;
;;; At (DEBUG 3), the debugger uses VAR wherever it is in scope, so
;;; every value VAR is given is kept: all its sets are converted, and
;;; every version gets a use, so that it has a TN for the debugger to
;;; find it in.
(defun ssa-convert-var (var)
  (catch 'ineligible
    (let* ((sets (lambda-var-sets var))
           (debugger-uses-p (policy (lambda-bind (lambda-var-home var))
                                (= debug 3)))
           (phis (compute-reaching-definitions var debugger-uses-p))
           (refs (lambda-var-refs var)))

      (setf (lambda-var-origin var) var)

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
              (new-var (make-ssa-var var)))
          (aver (= (length (block-pred block))
                   (length (phi-operands phi))))
          (setf (leaf-ever-used new-var) t)
          (setf (phi-new-var phi) new-var)
          (ssa-ensure-join-lambda block new-var)))

      (dolist (set (lambda-var-sets var))
        (when (or (node-lvar set) debugger-uses-p)
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

      (when debugger-uses-p
        (insert-keep-live var)
        (dolist (set sets)
          (insert-keep-live (set-var set)))
        (dolist (phi phis)
          (insert-keep-live (phi-new-var phi))))

      (dolist (ref refs)
        (change-ref-leaf ref (definition-ssa-var (ref-ssa-definition ref) var)))

      (note-ssa-version-scopes var)
      (mapc #'delete-set (lambda-var-sets var))

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

;;; Convert all eligible lambda vars bound in HOME to SSA form.
(defun ssa-convert (fun)
  (let ((converted nil))
    (flet ((frob (fun)
             (dolist (var (lambda-vars fun))
               (when (ssa-convertible-var-p var)
                 (when (ssa-convert-var var)
                   (setq converted t))))))
      (frob fun)
      (mapc #'frob (lambda-lets fun)))
    converted))

;;; Ask for COMPONENT to be rescanned for newly convertible
;;; variables. We request rather than convert on the spot as it is
;;; dangerous to do SSA conversion in the middle of IR1 optimizations,
;;; as conversion can hack the flow graph or create new lambdas.
(defun request-ssa-conversion (fun)
  (declare (type clambda fun))
  (setf (lambda-ssa-pending fun) t)
  (values))

;;; Note that VAR should be reconsidered for SSA conversion. This
;;; usually happens when a reference or set which was keeping it
;;; closed over has been deleted.
(defun note-ssa-candidate (var)
  (declare (type lambda-var var))
  (when (ssa-possible-candidate-p var)
    (let ((fun (lambda-home (lambda-var-home var))))
      (unless (eq (lambda-ssa-pending fun) t)
        (pushnew var (lambda-ssa-pending fun)))))
  (values))

;;; Note that conversion changed COMPONENT.
(defun note-ssa-converted (component)
  (setf (component-reanalyze component) t)
  (reoptimize-component component t)
  (values))

;;; Act on whatever was requested since the pass last ran.
(defun ssa-convert-pending (component)
  (declare (type component component))
  (let ((converted nil))
    (dolist (fun (component-lambdas component))
      (let ((pending (lambda-ssa-pending fun)))
        (when pending
          (setf (lambda-ssa-pending fun) nil)
          (cond ((eq pending t)
                 (when (ssa-convert fun)
                   (setq converted t)))
                (t
                 (dolist (var pending)
                   (cond ((ssa-convertible-var-p var)
                          (when (ssa-convert-var var)
                            (setq converted t)))
                         ((ssa-possible-candidate-p var)
                          (push var (lambda-ssa-pending fun))))))))))
    (when converted
      (note-ssa-converted component))
    (clear-flags component))
  (values))

;;; Convert COMPONENT to SSA form.
(defun ssa-convert-component (component)
  (declare (type component component))
  (dolist (fun (component-lambdas component))
    (request-ssa-conversion fun))
  (ssa-convert-pending component))
