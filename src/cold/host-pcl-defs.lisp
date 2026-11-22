;;;; Compile-time support for cross-compiling PCL.
;;;;
;;;; PCL's macros expand by calling ordinary PCL functions (EXPAND-DEFMETHOD,
;;;; MAKE-METHOD-LAMBDA-INTERNAL, the code walker, the DLISP emitters, ...).
;;;; The cross-compiler has those from PCL's files flagged :HOST-DEFINITIONS
;;;; in build-order.lisp-expr.  What is here decides what of PCL's files the
;;;; target gets, as genesis writes the CLOS that host PCL builds
;;;; (src/cold/host-pcl.lisp).

(in-package "SB-COLD")

(defparameter *host-pcl-proper*
  '("src/pcl/low" "src/pcl/macros" "src/pcl/ecd" "src/pcl/compiler-support"
    "src/pcl/defs" "src/pcl/wrapper" "src/pcl/class-init"
    "src/pcl/fngen" "src/pcl/cache" "src/pcl/dlisp" "src/pcl/dlisp2" "src/pcl/boot"
    "src/pcl/vector" "src/pcl/slots-boot" "src/pcl/combin" "src/pcl/dfun" "src/pcl/ctor"
    "src/pcl/braid" "src/pcl/dlisp3" "src/pcl/generic-functions" "src/pcl/slots"
    "src/pcl/init" "src/pcl/std-class" "src/pcl/cpl" "src/pcl/fsc" "src/pcl/methods"
    "src/pcl/fixup" "src/pcl/defcombin"))

(defvar *host-pcl-making-methods* nil)
(defvar *host-pcl-boot-seen* nil)
(defvar *host-pcl-stem* nil) ; the file being cross-compiled
(defun host-pcl-before-target-compile (stem)
  (setq *host-pcl-stem* stem)
  ;; host PCL makes the methods of all but PCL proper's from boot.lisp on,
  ;; which it loads itself (:EARLY: note-method-info in dump.lisp)
  (setq *host-pcl-making-methods*
        (cond ((string= stem "src/pcl/boot") (setq *host-pcl-boot-seen* t) nil)
              ((not *host-pcl-boot-seen*) :early)
              ((member stem *host-pcl-proper* :test #'string=) nil)
              (t t))))

;;; Each method's initargs (its plist: SLOT-NAME-LISTS etc.) are computed by
;;; the code walker along with its function, and they belong together: the
;;; dump of the function takes them along (FOP-MSET), and so do genesis and
;;; host PCL.  Methods of files whose bodies are the target's only
;;; (:PCL-TARGET-ONLY) are made from them.
(defun host-pcl-record-method-info ()
  (let ((real (fdefinition 'sb-pcl::make-method-initargs-form-internal1)))
    (setf (fdefinition 'sb-pcl::make-method-initargs-form-internal1)
          (lambda (initargs body req-args lmf-params restp)
            (when (and (boundp 'sb-pcl::*method-name*) sb-pcl::*method-name*)
              (sb-c::note-method-info `(sb-pcl::fast-method ,@sb-pcl::*method-name*)
                                      (copy-tree (car lmf-params))
                                      (copy-tree initargs)
                                      *host-pcl-making-methods*))
            (funcall real initargs body req-args lmf-params restp)))))

;;; Non-PCL target functions that PCL's macro expanders call.
(unless (fboundp 'sb-int:interned-symbol-p)
  (defun sb-int:interned-symbol-p (x) (and (symbolp x) (symbol-package x) t)))
(unless (fboundp 'sb-int:posq)
  (defun sb-int:posq (item list) (position item list :test #'eq)))
;;; In SB-PCL some CL names are the cross-compiler's strict versions, which
;;; it leaves undefined (set-up-cold-packages): PCL's code has the host's.
(dolist (name '("SYMBOL-PACKAGE" "PACKAGE-NAME"))
  (let ((s (find-symbol name "XC-STRICT-CL")))
    (when (and s (not (fboundp s)))
      (setf (fdefinition s) (fdefinition (find-symbol name "CL"))))))

;;; Target functions that macro bodies mention; the host compiles those
;;; bodies but only the target defines the functions.
(declaim (ftype function sb-mop:method-function sb-pcl::class-wrapper))

;;;; PCL for the target, as genesis writes the classes, generic functions,
;;;; methods and method combinations that host PCL makes
;;;; (src/cold/host-pcl.lisp).  Cross-compiled, their DEFCLASS, DEFGENERIC,
;;;; DEFMETHOD and DEFINE-METHOD-COMBINATION forms (pre-warm.lisp's
;;;; !DEFINITION) have their compile-time effects, and the rest of what
;;;; they expand into is compiled into a function that is never called,
;;;; which dumps the methods' functions (FOP-MSET) for host PCL's methods.
;;;; Those and the toplevel forms pre-warm.lisp's markers say wait are
;;;; recorded, for host PCL to run.

;;; (kind stem package form), last first
(defvar *host-pcl-record* nil)
(defun sb-pcl::!xc-record (kind form expansion)
  `(progn (eval-when (:compile-toplevel)
            (push (list ',kind *host-pcl-stem* *package* ',form) *host-pcl-record*))
          ,expansion))

(defun host-pcl-compile-time-only (expansion)
  `(progn
     ,@(loop for x in (if (typep expansion '(cons (eql progn))) (cdr expansion) (list expansion))
             when (and (typep x '(cons (eql eval-when) (cons list)))
                       (member :compile-toplevel (second x)))
               collect `(eval-when (:compile-toplevel) ,@(cddr x)))
     (sb-pcl::!host-form (lambda () ,expansion))))
(defun sb-pcl::!xc-definition (kind form expansion)
  (sb-pcl::!xc-record kind form (host-pcl-compile-time-only expansion)))

(host-pcl-record-method-info)
