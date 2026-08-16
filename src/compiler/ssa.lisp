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
;;; NODE. If there is none, return NIL
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

;;; Find the definition of VAR reaching the end of BLOCK.
(defun find-block-end-definition (var block)
  (declare (type lambda-var var)
           (type cblock block))
  (or (find-block-local-definition var (block-last block))
      (find-block-start-definition var block)))

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
;;; later. We return VAR in this case as a sentinel value.
(defun find-block-start-definition (var block)
  (declare (type lambda-var var)
           (type cblock block))
  (let* ((preds (block-pred block))
         (flag (block-flag block))
         (home (lambda-home (lambda-var-home var))))
    ;; TODO: We can't SSA convert variables in the presence of
    ;; non-local control flow. Consider the following example:
    ;;
    ;; (lambda (n)
    ;;   (let ((x 0))
    ;;     (catch 'tag
    ;;       (setq x 1)
    ;;       (when (plusp n) (throw 'tag :early))
    ;;       (setq x 2))
    ;;     x))
    (unless (eq (block-home-lambda block) home)
      (throw 'ineligible nil))
    (cond ((null preds)
           var)
          ((and (phi-p flag)
                (eq (phi-var flag) var))
           flag)
          ((eq flag var)
           (let ((phi (make-phi var block)))
             (setf (block-flag block) phi)
             phi))
          ((and (consp flag)
                (eq (car flag) var))
           (cdr flag))
          (t
           (setf (block-flag block) var)
           (collect ((operands))
             (let ((definition (find-block-end-definition var (first preds)))
                   (same t))
               (operands definition)
               (dolist (pred (rest preds))
                 (let ((pred-definition (find-block-end-definition var pred)))
                   (operands pred-definition)
                   (unless (eq pred-definition definition)
                     (setq same nil))))
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

;;; Find the definition of VAR reaching REF.
(defun find-ref-definition (var ref)
  (declare (type lambda-var var)
           (type ref ref))
  (or (find-block-local-definition var ref)
      (find-block-start-definition var (node-block ref))))

;;; Eliminate any superfluous phis in PHIS by finding strongly
;;; connected components with Tarjan's SCC algorithm and setting
;;; definition forwarding pointers appropriately. In particular, an
;;; SCC where there is only one reaching definition from outside of
;;; the component is collapsed by forwarding each phi in the component
;;; to that definition. An SCC which has no reaching definitions from
;;; outside the SCC comes from dead code and can be ignored.
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
                   (unless (member resolved scc)
                     (pushnew resolved outer-operands)))))
             (when (and outer-operands
                        (null (rest outer-operands)))
               (let ((outer-operand (first outer-operands)))
                 (dolist (node scc)
                   (setf (phi-replacement node) outer-operand)))))))
      (dolist (phi phis)
        (when (and (null (phi-replacement phi))
                   (= (phi-index phi) -1))
          (strong-connect phi))))))

;;; Compute reaching definitions for VAR, annotating each of VAR's
;;; references with its reaching definition. A definition may be:
;;; -- VAR itself, representing the initial variable binding.
;;; -- A set node representing the value after the assignment.
;;; -- A phi representing a value-join.
;;;
;;; We do this by walking the graph backward from each of VAR's
;;; references. Since this process creates many phis which may be
;;; trivial or redundant, we do a post-pass using Tarjan's SCC
;;; algorithm to eliminate them. (See section 3.2 of Braun et
;;; al. 2013). In addition, single operand phis in single predecessor
;;; blocks are also eliminated here. The set of phis which survive is
;;; returned as a list.
(defun compute-reaching-definitions (var)
  (declare (type lambda-var var))
  (let ((*created-phis* '()))
    (dolist (ref (leaf-refs var))
      (setf (ref-ssa-definition ref)
            (find-ref-definition var ref)))

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
          (phis phi)
          (setf (phi-operands phi)
                (mapcar #'resolve-definition (phi-operands phi)))))
      (phis))))

;;; Make a fresh version of VAR.
(defun ssa-new-var-version (var)
  (declare (lambda-var var))
  (make-lambda-var (gensym (string (leaf-%source-name var)))
                   :type (leaf-type var)))

;;; Returns the SSA variable representing the value of this definition
;;; once all definitions have been SSA converted.
(defun definition-ssa-var (definition)
  (etypecase definition
    (lambda-var definition)
    (cset (set-var definition))
    (phi (phi-new-var definition))))

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
               (new-var (ssa-new-var-version var))
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
    (let ((phis (compute-reaching-definitions var))
          (refs (lambda-var-refs var)))

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
              (new-var (ssa-new-var-version var)))
          (aver (= (length (block-pred block))
                   (length (phi-operands phi))))
          (setf (leaf-ever-used new-var) t)
          (setf (phi-new-var phi) new-var)
          (ssa-ensure-join-lambda block new-var)))

      (dolist (set (lambda-var-sets var))
        (cond ((node-lvar set)
               (ssa-convert-set set var))
              (t
               (flush-dest (set-value set))
               (setf (lambda-var-sets var) (delq1 set (lambda-var-sets var)))
               (unlink-node set))))

      (dolist (phi phis)
        (let* ((block (phi-block phi))
               (fun (bind-lambda (block-start-node block)))
               (new-var (phi-new-var phi))
               (n (position new-var (lambda-vars fun))))
          (loop for operand in (phi-operands phi)
                for pred in (block-pred block)
                do (ssa-ensure-pred-local-calls pred block fun
                                                (definition-ssa-var operand)
                                                n))))

      (dolist (ref refs)
        (change-ref-leaf ref (definition-ssa-var (ref-ssa-definition ref))))

      t)))

;;; Can VAR be converted? Closed-over and special variables keep their
;;; CSETs: the first because its value lives in an environment several
;;; functions share, the second because the set is a store to a symbol.
(defun ssa-convertible-var-p (var)
  (and (lambda-var-sets var)
       (leaf-refs var)
       (not (closure-var-p var))
       (not (lambda-var-specvar var))
       (not (lambda-var-indirect var))
       (lambda-bind (lambda-var-home var))))

;;; Could VAR still become convertible later? Every test in
;;; SSA-CONVERTIBLE-VAR-P except CLOSURE-VAR-P is settled once and for
;;; all, or can only go the wrong way: sets and references are never
;;; added back, SPECVAR is fixed at creation, and INDIRECT is only ever
;;; set. So a variable that fails one of those is done with, while one
;;; that is merely still closed over may yet be promoted.
(defun ssa-possible-candidate-p (var)
  (and (lambda-var-sets var)
       (leaf-refs var)
       (not (lambda-var-specvar var))
       (not (lambda-var-indirect var))
       (let ((home (lambda-var-home var)))
         (and home
              (not (functional-kind-eq home deleted zombie))
              (lambda-bind home)))))

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

;;; Ask for COMPONENT to be rescanned for newly convertible variables.
;;; Requesting rather than converting on the spot is deliberate: the
;;; callers run from inside IR1 optimization, where the component is
;;; part way through some other transformation, and this pass splices
;;; blocks and creates lambdas.
(defun request-ssa-conversion (fun)
  (declare (type clambda fun))
  (setf (lambda-ssa-pending fun) t)
  (values))

;;; Ask for VAR alone to be reconsidered: deleting a reference or a set
;;; can have removed the one that was keeping VAR closed over.
(defun note-ssa-candidate (var)
  (declare (type lambda-var var))
  (let* ((home (lambda-var-home var))
         (fun (and home (lambda-home home))))
    (when (and fun
               (not (functional-kind-eq fun deleted zombie))
               (lambda-bind fun))
      (unless (eq (lambda-ssa-pending fun) t)
        (pushnew var (lambda-ssa-pending fun)))))
  (values))

;;; Note that conversion changed COMPONENT.
(defun note-ssa-converted (component)
  ;; The new blocks were spliced into the DFO by hand. Ask for it to be
  ;; recomputed so that FIND-DFO links whatever is now an entry and
  ;; CLEAN-COMPONENT drops whatever is now unreachable, rather than
  ;; leaving either for a later pass to trip over.
  (setf (component-reanalyze component) t)
  ;; T, not :MAYBE. The new lambdas carry no type information of their
  ;; own -- what the variable could hold has to reach the join
  ;; parameters through PROPAGATE-LOCAL-CALL-ARGS -- and that needs
  ;; another optimization round over the whole component.
  (reoptimize-component component t)
  (values))

;;; Act on whatever was requested since the pass last ran. Called from
;;; the optimization driver, which is a point where the component is
;;; whole.
;;;
;;; A queued variable that is not convertible yet stays queued rather
;;; than being dropped. The request records that something about the
;;; variable moved, not that it is ready: the reference whose deletion
;;; prompted the request is often not the last one keeping the variable
;;; closed over, and by the time it is, nothing would ask again.
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
  (let ((converted nil))
    (dolist (fun (component-lambdas component))
      (when (ssa-convert fun)
        (setq converted t)))
    (when converted
      (note-ssa-converted component)))
  (clear-flags component)
  (values))
