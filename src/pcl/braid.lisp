;;;; bootstrapping the meta-braid
;;;;
;;;; The code in this file takes the early definitions that have been
;;;; saved up and actually builds those class objects. This work is
;;;; largely driven off of those class definitions, but the fact that
;;;; STANDARD-CLASS is the class of all metaclasses in the braid is
;;;; built into this code pretty deeply.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; copyright information from original PCL sources:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.

(in-package "SB-PCL")

(defun class-of (x)
  (declare (explicit-check))
  (wrapper-class (layout-of x)))

(defun eval-form (form)
  (template-lambda (eval-form) (form) () (eval form)))

(defun condition-class-direct-slot-initargs (classoid)
  (mapcar (lambda (slot)
            `(:name ,(condition-slot-name slot)
              :initargs ,(condition-slot-initargs slot)
              :readers ,(condition-slot-readers slot)
              :writers ,(condition-slot-writers slot)
              ,@(when (condition-slot-initform-p slot)
                  (let ((initform (condition-slot-initform slot))
                        (initfun (condition-slot-initfunction slot)))
                    `(:initform ',initform :initfunction ,initfun)))
              :allocation ,(condition-slot-allocation slot)
              :documentation ,(condition-slot-documentation slot)))
          (condition-classoid-slots classoid)))

(defun ensure-non-standard-class (name classoid &optional existing-class extra-data)
  (labels
      ((ensure (metaclass slots)
         (let ((supers (mapcar #'classoid-name (classoid-direct-superclasses classoid))))
           (ensure-class-using-class existing-class name
                                     :metaclass metaclass :name name
                                     :direct-superclasses supers
                                     :direct-slots slots)))
       (slot-initargs-from-structure-slotd (slotd writer-fn reader-fn)
         (flet ((name->fun (f) (if (functionp f) f (fdefinition f))))
           `(:name ,(dsd-name slotd)
             :defstruct-accessor-symbol ,(dsd-accessor-name slotd)
             :internal-reader-function ,(name->fun reader-fn)
             :internal-writer-function ,(name->fun writer-fn)
             :always-bound-p ,(dsd-always-boundp slotd)
             :type ,(dsd-type slotd)
             :initform ,(dsd-default slotd)
             ;; This is nuts! any DEFAULT might need its lexical environment,
             ;; yet we EVAL in the null environment.
             :initfunction ,(eval-form (dsd-default slotd)))))
       (accessor-closures (dsd dd)
         ;; The slot's accessors from its primitives, for a structure whose
         ;; DEFSTRUCT gave none (*STRUCT-ACCESS-FRAGMENTS*): one known only at
         ;; compile time, or one whose class genesis writes.
         (multiple-value-bind (reader-fn writer-fn) (dsd-primitives dsd dd)
           (let ((index (dsd-index dsd)))
             (list (template-lambda (structure-slot-writer) (writer-fn index) (newval object)
                     (funcall writer-fn object index newval)
                     newval)
                   (template-lambda (structure-slot-reader) (reader-fn index) (object)
                     (funcall reader-fn object index))))))
       (structure-type-slot-description-list (type)
         (let* ((dd (find-defstruct-description type))
                (include (dd-include dd))
                (all-slots (dd-slots dd)))
           (unless extra-data
             (dolist (dsd (dd-slots dd))
               (push (cons dsd (accessor-closures dsd dd)) extra-data)))
           (multiple-value-bind (super slot-overrides)
               (if (consp include)
                   (values (car include) (mapcar #'car (cdr include)))
                   (values include nil))
             (let ((included-slots
                    (when super
                      (dd-slots (find-defstruct-description super)))))
               ;; This seems like a very unclear way to do what it's doing, which is
               ;; collect slots of TYPE that are not in its direct ancestor, *or*
               ;; which have an altered definition relative to the inherited one.
               (loop for slot = (pop all-slots)
                  for included-slot = (pop included-slots)
                  while slot
                  when (or (not included-slot)
                           (member (dsd-name included-slot) slot-overrides :test #'eq))
                  collect (apply #'slot-initargs-from-structure-slotd
                                 (assoc slot extra-data))))))))
    (cond ((structure-type-p name)
           (ensure 'structure-class (structure-type-slot-description-list name)))
          ((condition-type-p name)
           (ensure 'condition-class
                   (condition-class-direct-slot-initargs classoid)))
          (t
           (error "~@<~S is not the name of a class.~@:>" name)))))

(defun ensure-deffoo-class (classoid &optional accessors)
  (let ((class (classoid-pcl-class classoid)))
    (cond (class
           (ensure-non-standard-class (class-name class) classoid class accessors))
          ((pcl-ready-p)
           (ensure-non-standard-class (classoid-name classoid) classoid nil accessors)))))

(defun ensure-defstruct-class (classoid)
  ;; Create an association from the DSD to the reader and writer functions.
  (let* ((name (classoid-name classoid))
         (dd (find-defstruct-description name))
         (fragments sb-kernel::*struct-access-fragments*))
    (collect ((accessors))
      (dolist (dsd (dd-slots dd))
        (accessors (list dsd (pop fragments) (pop fragments))))
      (ensure-deffoo-class classoid (accessors)))))

;;; Prior to the normal (steady-state) defstruct hook getting installed,
;;; we just accumulate an alist of slot accessor functions keyed by
;;; the type name. After switching it over, we start calling
;;; ENSURE-CLASS-USING-CLASS. But that means if PCL isn't ready to
;;; actually create classes, we can't switch over.  So we refrain
;;; from installing the DEFSTRUCT-HOOK until there are no more
;;; defstruct forms to execute within PCL itself.
(!after-pcl-bootstrap (pushnew 'ensure-deffoo-class sb-kernel::*define-condition-hooks*))

(define-load-time-global *simple-stream-root-classoid* :unknown)

(defun set-bitmap-and-flags (layout &aux (inherits (layout-inherits layout))
                                         (flags (layout-flags layout)))
  (when (eq (layout-classoid layout) *simple-stream-root-classoid*)
    (setq flags (logior flags +simple-stream-layout-flag+)))
  ;; We decide only at class finalization time whether it is funcallable.
  ;; Picking the right bitmap could probably be done sooner given the metaclass,
  ;; but this approach avoids changing how PCL uses MAKE-LAYOUT.
  ;; FIXME: we should assert that this is not a STRUCTURE-OBJECT,
  ;; because there must not be additional ID words preceding the bitmap.
  ;; STANDARD-OBJECT can't generally use a fixed number of ID slots
  ;; because of arbitrary changes to inheritance being permissible.
  ;; On the other hand, we do create a new layout for each change
  ;; to superclasses, so maybe it's possible.
  (dovector (ancestor inherits)
    (when (eq ancestor #.(find-layout 'function))
      (%raw-instance-set/signed-word layout (sb-kernel::type-dd-length layout)
                                     sb-kernel::funinstance-layout-bitmap))
    (setq flags (logior (logand (logior +sequence-layout-flag+
                                        +stream-layout-flag+
                                        +simple-stream-layout-flag+
                                        +file-stream-layout-flag+
                                        +string-stream-layout-flag+)
                                (layout-flags ancestor))
                        flags)))
  (setf (layout-flags layout) flags))

;;; Set the inherits from CPL, and register the layout. This actually
;;; installs the class in the Lisp type system.
(defun %update-lisp-class-layout (class wrapper)
  ;; Protected by **world-lock** in callers.
  (let ((classoid (layout-classoid wrapper)))
    (unless (eq (classoid-layout classoid) wrapper)
      (set-layout-inherits wrapper
                           (order-layout-inherits
                            (map 'simple-vector #'class-wrapper
                                 (reverse (rest (class-precedence-list class)))))
                           nil 0)
      (set-bitmap-and-flags wrapper)
      (register-layout wrapper :invalidate t)

      ;; FIXME: I don't think this should be necessary, but without it
      ;; we are unable to compile (TYPEP foo '<class-name>) in the
      ;; same file as the class is defined.  If we had environments,
      ;; then I think the classsoid whould only be associated with the
      ;; name in that environment...  Alternatively, fix the compiler
      ;; so that TYPEP foo '<class-name> is slow but compileable.
      (let ((name (class-name class)))
        (when (and name (symbolp name) (eq name (classoid-name classoid)))
          (setf (find-classoid name) classoid))))))

#+sb-xc-host
(!capturing
(loop for (name . x)
      in (let (classoid-cells)
           (do-all-symbols (s classoid-cells)
             (let ((cell (sb-int:info :type :classoid-cell s)))
               (when cell
                 (push (cons s cell) classoid-cells)))))
      do
  (when (classoid-cell-pcl-class x)
    (let* ((class (find-class-from-cell name x))
           (layout (class-wrapper class))
           (lclass (layout-classoid layout))
           (lclass-pcl-class (classoid-pcl-class lclass))
           (olclass (find-classoid name nil)))
      (if lclass-pcl-class
          (aver (eq class lclass-pcl-class))
          (setf (classoid-pcl-class lclass) class))

      (%update-lisp-class-layout class layout)

      (cond (olclass
             (aver (eq lclass olclass)))
            (t
             (setf (find-classoid name) lclass)))))))


(define-condition effective-method-condition (reference-condition)
  ((generic-function :initarg :generic-function
                     :reader effective-method-condition-generic-function)
   (method :initarg :method :initform nil
           :reader effective-method-condition-method)
   (args :initarg :args
         :reader effective-method-condition-args))
  (:default-initargs
   :generic-function (missing-arg)
   :args (missing-arg)))

(define-condition effective-method-error (error
                                          effective-method-condition)
  ((problem :initarg :problem :reader effective-method-error-problem))
  (:default-initargs :problem (missing-arg))
  (:report
   (lambda (condition stream)
     (format stream "~@<~A for the generic function ~2I~_~S ~I~_when ~
                     called ~@[from method ~2I~_~S~I~_~]with arguments ~
                     ~2I~_~S.~:>"
             (effective-method-error-problem condition)
             (effective-method-condition-generic-function condition)
             (effective-method-condition-method condition)
             (effective-method-condition-args condition)))))

(define-condition no-applicable-method-error (effective-method-error)
  ()
  (:default-initargs
   :problem "There is no applicable method"
   :references '((:ansi-cl :section (7 6 6)))))
(def!method no-applicable-method (generic-function &rest args)
  (error 'no-applicable-method-error
         :generic-function generic-function
         :args args))

(define-condition no-next-method-error (effective-method-error)
  ()
  (:default-initargs
   :problem "There is no next method"
   :references '((:ansi-cl :section (7 6 6 2)))))
(def!method no-next-method ((generic-function standard-generic-function)
                           (method standard-method) &rest args)
  (error 'no-next-method-error
         :generic-function generic-function
         :method method
         :args args))

;;; An extension to the ANSI standard: in the presence of e.g. a
;;; :BEFORE method, it would seem that going through
;;; NO-APPLICABLE-METHOD is prohibited, as in fact there is an
;;; applicable method.  -- CSR, 2002-11-15
(define-condition no-primary-method-error (effective-method-error)
  ()
  (:default-initargs
   :problem "There is no primary method"
   :references '((:ansi-cl :section (7 6 6 2)))))
(def!method no-primary-method (generic-function &rest args)
  (error 'no-primary-method-error
         :generic-function generic-function
         :args args))

;; FIXME shouldn't this specialize on STANDARD-METHOD-COMBINATION?
(def!method invalid-qualifiers ((gf generic-function)
                               combin
                               method)
  (let* ((qualifiers (method-qualifiers method))
         (qualifier (first qualifiers))
         (type-name (method-combination-type-name combin))
         (why (cond
                ((cdr qualifiers)
                 "has too many qualifiers")
                (t
                 (aver (not (standard-method-combination-qualifier-p
                             qualifier)))
                 "has an invalid qualifier"))))
    (invalid-method-error
     method
     "~@<The method ~S on ~S ~A.~
      ~@:_~@:_~
      ~@(~A~) method combination requires all methods to have one of ~
      the single qualifiers ~{~S~#[~; and ~:;, ~]~} or to have no ~
      qualifier at all.~@:>"
     method gf why type-name +standard-method-combination-qualifiers+)))

(def!method compute-primary-methods ((gf generic-function)
                                    combin
                                    applicable-methods)
  applicable-methods)
