;;;; PCL's objects on the host, as the host's PCL (src/cold/host-pcl.lisp)
;;;; makes them for genesis to write, and the cross-compiler's operators on
;;;; instances and funcallable instances, which know them.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-PCL")

;;; A standard instance: its layout (one of PCL's, see
;;; src/code/early-classoid.lisp) and its slots.  Not a host structure, as
;;; a standard instance is no STRUCTURE-OBJECT.
(cl:defclass host-instance ()
  ((wrapper :initarg :wrapper :accessor host-instance-wrapper)
   (slots :initarg :slots :accessor host-instance-slots)))
(defun %make-host-instance (wrapper slots)
  (cl:make-instance 'host-instance :wrapper wrapper :slots slots))
(defun host-instance-p (x) (typep x 'host-instance))

;;; A funcallable object of the target (funcallable instance or funcallable
;;; structure) is a host closure that calls the FUNCTION of its record.
(defstruct (host-funcallable (:constructor nil))
  (function nil))

(defstruct (host-fin (:include host-funcallable)
                     (:constructor %make-host-fin (wrapper slots hash target-function)))
  wrapper slots hash
  target-function)    ; what the target's funcallable instance starts with

;;; closure -> record
(defvar *host-funcallables* (make-hash-table :test 'eq))
(defun host-funcallable-record (x) (and (functionp x) (gethash x *host-funcallables*)))
(defun host-fin-record (x)
  (let ((record (host-funcallable-record x))) (and (host-fin-p record) record)))

;;; The cross-compiler's declarations in PCL's DEFMETHODs (boot.lisp), which
;;; the host evaluates
(cl:proclaim '(declaration sb-c::source-form sb-c::current-defmethod))

;;; NAMED-LAMBDA (cross-misc.lisp): the host keeps the name, by which genesis
;;; finds the target's function (methods' fast functions)
(defvar *host-fun-names* (make-hash-table :test 'eq))
(defun host-named (name function)
  (setf (gethash function *host-fun-names*) name)
  function)

;;; host instances of target structures: the cross-compiler's own (layouts,
;;; classoids, ...) and PCL's (caches, ...)
(defun host-target-structure-p (x)
  (and (typep x 'structure-object)
       (not (host-instance-p x))
       (sb-kernel:find-classoid (type-of x) nil)
       t))

(defun host-built-in-class-name (x)
  (etypecase x
    (null 'null)
    (symbol 'symbol)
    (cons 'cons)
    (fixnum 'fixnum)
    (integer 'bignum)
    (ratio 'ratio)
    (single-float 'single-float)
    (double-float 'double-float)
    (complex 'complex)
    (character 'character)
    (simple-vector 'simple-vector)
    (simple-base-string 'simple-base-string)
    (simple-string 'simple-character-string)
    (hash-table 'hash-table)
    (simple-bit-vector 'simple-bit-vector)
    (vector 'vector)
    (function 'function)))

(defun sb-kernel:layout-of (x)
  (let ((record (host-funcallable-record x)))
    (cond ((host-instance-p x) (host-instance-wrapper x))
          ((host-fin-p record) (host-fin-wrapper record))
          (t (sb-kernel:classoid-layout
              (sb-kernel:find-classoid
               (cond (record (get (type-of record) 'host-structure-name))
                     ((host-target-structure-p x) (type-of x))
                     (t (host-built-in-class-name x)))))))))

(defun (setf sb-kernel:%funcallable-instance-fun) (function x)
  (setf (host-funcallable-function
         (or (host-funcallable-record x) (error "host PCL: not funcallable: ~S" x)))
        function))
;;; The slot vector of a funcallable proxy, as low.lisp's FSC-INSTANCE-SLOTS
;;; reads it (a standard instance's: %INSTANCE-REF in defstruct.lisp)
(defun sb-kernel:%funcallable-instance-info (fin index)
  (assert (= index 0))
  (host-fin-slots (host-fin-record fin)))

;;; A standard instance as class-init.lisp's ALLOCATE-STANDARD-INSTANCE makes
;;; one, its slot vector at INSTANCE-DATA-START (%NEW-INSTANCE is the
;;; cross-compiler's macro only)
(defun sb-kernel::%new-instance (layout size)
  (declare (ignore size))
  (%make-host-instance layout nil))
(defun sb-kernel:%instance-set (instance index value)
  (declare (ignore index))
  (assert (host-instance-p instance))
  (setf (host-instance-slots instance) value))

;;; Layouts of PCL's classes (src/code/early-classoid.lisp), as wrapper.lisp
;;; and braid.lisp set them up
(defun sb-kernel::set-layout-inherits (layout inherits structurep this-id)
  (declare (ignore structurep this-id))
  (setf (sb-kernel:layout-inherits layout) inherits))
(defun sb-kernel:%raw-instance-set/signed-word (layout index value) ; the bitmap
  (declare (ignore index))
  (setf (sb-kernel::layout-%bitmap layout) value))

;;; PCL's code walker works on the cross-compiler's lexical environments, and
;;; host code has host environments.  Method bodies on the host need only be
;;; correct, not optimized: the target gets its own, cross-compiled method
;;; functions.  So the host walks a method body as a tree: it rewrites
;;; CALL-NEXT-METHOD and notices SETQs, without expanding macros, and tells
;;; nothing about declarations, which keeps slot access unoptimized.  fngen.lisp
;;; walks the same way, over generated code, which has no macros of its own.
(defun host-walk-form (form env walk-function)
  (labels ((walk (form)
             (cond ((atom form) form)
                   ((eq (car form) 'quote) form)
                   ((and (eq (car form) 'setq) (consp (cdr form)))
                    (loop for (var value) on (cdr form) by #'cddr
                          do (funcall walk-function var :set env))
                    (cons 'setq (walk-list (cdr form))))
                   ;; slot access stays unoptimized
                   ((member (car form) '(sb-xc:slot-value set-slot-value
                                         sb-xc:slot-boundp sb-xc:slot-makunbound))
                    (cons (car form) (walk-list (cdr form))))
                   (t
                    ;; a true second value means "don't walk the result"
                    (multiple-value-bind (new done) (funcall walk-function form :eval env)
                      (if (and (consp new) (not done))
                          ;; a LET binding, ((LAMBDA ...) ...): a form too
                          (cons (if (consp (car new)) (walk (car new)) (car new))
                                (walk-list (cdr new)))
                          new)))))
           (walk-list (list)
             (if (consp list)
                 (cons (walk (car list)) (walk-list (cdr list)))
                 list)))
    (walk form)))

;;;; Target functions that PCL's code calls, which the host lacks

;;; Target instances, not the host's own structures (its lexical
;;; environments, ...), which the cross-compiler's %INSTANCEP counts too
(defun host-%instancep (x) (or (host-instance-p x) (host-target-structure-p x)))

;;; Locks: the host is single-threaded here.  (More in cross-misc.lisp,
;;; for the cross-compiler's copy of PCL's files too.)
(defvar sb-kernel::**world-lock** nil)
(defun sb-thread:holding-mutex-p (mutex) (declare (ignore mutex)) t)

;;; &REST and &MORE primitives of generated code: a more-context is the
;;; rest list itself.
(defun sb-c::%rest-list (list) list)
(defun sb-kernel:fast-&rest-nth (n list) (nth n list))
(defun sb-c::%rest-context (list) (values list (length list)))
(defun sb-c::%more-arg (context index) (nth index context))
(defun sb-c::%odd-key-args-error () (error "odd number of &KEY arguments"))
(defun sb-c::%unknown-key-arg-error (key &optional context)
  (declare (ignore context))
  (error "unknown &KEY argument: ~S" key))
(defun sb-int:hash-table-lock (table) (declare (ignore table)) nil)

(defun sb-int:sys-tlab-append (a b) (append a b))
(defun sb-int:sys-tlab-adjoin-eq (item list) (adjoin item list :test #'eq))

;;; Weak pointers only the target can make: genesis writes none.
(defstruct (host-weak-pointer (:constructor %make-host-weak-pointer (value)))
  value)
(defun sb-ext:make-weak-pointer (value) (%make-host-weak-pointer value))
(defun sb-ext:weak-pointer-value (wp)
  (let ((value (host-weak-pointer-value wp))) (values value (and value t))))

;;; for restarts' :INTERACTIVE (std-class.lisp), which the cross-compiler lacks
(defun sb-int:read-evaluated-form (&optional (prompt-control nil promptp) &rest prompt-args)
  (apply #'format *query-io* (if promptp prompt-control "~&Type a form to be evaluated: ")
         prompt-args)
  (finish-output *query-io*)
  (list (eval (read *query-io*))))

;;; layout name -> slot mapper (INSTALL-STRUCT-SLOT-MAPPER), for layouts that
;;; are not PCL's (STRUCTURE-OBJECT etc.), which genesis installs
(defvar *host-layout-slot-mappers* nil)

;;; The target's unprintable markers, as PCL proper makes them (slots.lisp's
;;; LOAD-TIME-VALUE) when the host loads it
(defun sb-int:make-unprintable-object (string) (list :unprintable string))

;;; The types of PCL's classes, to the host: what host PCL says of its
;;; objects (HOST-PCL-CLASS-TYPEP in src/cold/host-pcl.lisp), and of no
;;; other object.
(defvar *host-pcl-class-typep* nil)
(defun host-pcl-typep (x name)
  (and *host-pcl-class-typep* (funcall *host-pcl-class-typep* x name)))
(defun define-host-pcl-class-type (name)
  (let ((predicate (intern (format nil "HOST-PCL-TYPEP/~A:~A"
                                   (cl:package-name (cl:symbol-package name)) (symbol-name name))
                           "SB-PCL")))
    (setf (fdefinition predicate) (lambda (x) (host-pcl-typep x name)))
    (eval `(deftype ,name () '(satisfies ,predicate)))
    name))
(mapc #'define-host-pcl-class-type
      '(sb-xc:standard-object sb-xc:generic-function sb-xc:standard-generic-function
        sb-xc:method sb-xc:standard-method sb-xc:method-combination sb-xc:class
        sb-xc:built-in-class sb-xc:standard-class sb-xc:structure-class))
