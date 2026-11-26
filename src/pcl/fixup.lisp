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

;;; The methods that are PCL's own functions (*!GENERIC-FUNCTION-FIXUPS*)
(macrolet ((def (name qualifiers lambda-list function)
             (let ((rest (member '&rest lambda-list))
                   (vars (mapcar (lambda (x) (if (consp x) (car x) x))
                                 (ldiff lambda-list (member-if (lambda (x) (member x '(&rest &key)))
                                                               lambda-list)))))
               `(def!method ,name ,@qualifiers ,lambda-list
                  ,(if rest
                       `(apply #',function ,@(remove '&optional vars) ,(second rest))
                       `(,function ,@(remove '&optional vars)))))))
  (def add-method () ((generic-function standard-generic-function) (method method))
    real-add-method)
  (def remove-method () ((generic-function standard-generic-function) (method method))
    real-remove-method)
  (def get-method () ((generic-function standard-generic-function) qualifiers specializers
                      &optional (errorp t))
    real-get-method)
  (def ensure-generic-function-using-class ()
      ((generic-function generic-function) fun-name &rest options &key &allow-other-keys)
    real-ensure-gf-using-class--generic-function)
  (def ensure-generic-function-using-class ()
      ((generic-function null) fun-name &rest options &key &allow-other-keys)
    real-ensure-gf-using-class--null)
  (def make-method-lambda ()
      ((proto-generic-function standard-generic-function) (proto-method standard-method)
       lambda-expression environment)
    real-make-method-lambda)
  (def make-method-lambda-using-specializers ()
      ((proto-generic-function standard-generic-function) (proto-method standard-method)
       qualifiers specializers lambda-expression environment)
    real-make-method-lambda-using-specializers)
  (def make-method-specializers-form ()
      ((proto-generic-function standard-generic-function) (proto-method standard-method)
       specializer-names environment)
    real-make-method-specializers-form)
  (def make-specializer-form-using-class (or)
      ((proto-generic-function standard-generic-function) (proto-method standard-method)
       specializer-name environment)
    real-make-specializer-form-using-class/t)
  (def make-specializer-form-using-class (or)
      ((proto-generic-function standard-generic-function) (proto-method standard-method)
       (specializer-name specializer) environment)
    real-make-specializer-form-using-class/specializer)
  (def make-specializer-form-using-class (or)
      ((proto-generic-function standard-generic-function) (proto-method standard-method)
       (specializer-name symbol) environment)
    real-make-specializer-form-using-class/symbol)
  (def make-specializer-form-using-class (or)
      ((proto-generic-function standard-generic-function) (proto-method standard-method)
       (specializer-name cons) environment)
    real-make-specializer-form-using-class/cons)
  (def specializer-type-specifier ()
      ((proto-generic-function standard-generic-function) (proto-method standard-method)
       (specializer specializer))
    real-specializer-type-specifier/specializer)
  (def specializer-type-specifier ()
      ((proto-generic-function standard-generic-function) (proto-method standard-method)
       (specializer symbol))
    real-specializer-type-specifier/symbol)
  (def specializer-type-specifier ()
      ((proto-generic-function standard-generic-function) (proto-method standard-method)
       specializer)
    real-specializer-type-specifier/t)
  (def specializer-type-specifier ()
      ((proto-generic-function standard-generic-function) (proto-method standard-method)
       (specializer class-eq-specializer))
    real-specializer-type-specifier/class-eq-specializer)
  (def specializer-type-specifier ()
      ((proto-generic-function standard-generic-function) (proto-method standard-method)
       (specializer eql-specializer))
    real-specializer-type-specifier/eql-specializer)
  (def specializer-type-specifier ()
      ((proto-generic-function standard-generic-function) (proto-method standard-method)
       (specializer structure-class))
    real-specializer-type-specifier/structure-class)
  (def specializer-type-specifier ()
      ((proto-generic-function standard-generic-function) (proto-method standard-method)
       (specializer system-class))
    real-specializer-type-specifier/system-class)
  (def specializer-type-specifier ()
      ((proto-generic-function standard-generic-function) (proto-method standard-method)
       (specializer class))
    real-specializer-type-specifier/class)
  (def parse-specializer-using-class ()
      ((generic-function standard-generic-function) specializer)
    real-parse-specializer-using-class)
  (def unparse-specializer-using-class ()
      ((generic-function standard-generic-function) specializer)
    real-unparse-specializer-using-class)
  (def make-method-initargs-form ()
      ((proto-generic-function standard-generic-function) (proto-method standard-method)
       lambda-expression lambda-list environment)
    real-make-method-initargs-form)
  (def compute-effective-method ()
      ((generic-function generic-function) (combin standard-method-combination)
       applicable-methods)
    standard-compute-effective-method)
  (def compute-effective-method ()
      ((generic-function generic-function) (combin short-method-combination)
       applicable-methods)
    short-compute-effective-method))

;;; Replaces the early definition in slots-boot.lisp (by name, as genesis
;;; installs every DEFUN up front).
(defun real-ensure-accessor (fun-name) ; Make FUN-NAME exist as a GF if it doesn't
  (destructuring-bind (slot-name method) (cddr fun-name)
    (let ((reader-specializers (load-time-value (list (find-class 'slot-object)) t))
          (writer-specializers (load-time-value (list (find-class 't)
                                                      (find-class 'slot-object)) t))
          (fallback-reader-specializers
            (load-time-value (list (find-class 't)) t))
          (fallback-writer-specializers
            (load-time-value (list (find-class 't) (find-class 't)) t)))
      (multiple-value-bind (lambda-list specializers method-class initargs doc
                            fallback-initargs
                            fallback-specializers)
          (ecase method
            (reader
             (values '(object) reader-specializers 'global-reader-method
                     (make-std-reader-method-function 'slot-object slot-name)
                     "automatically-generated reader method"
                     (make-fallback-reader-method-function slot-name)
                     fallback-reader-specializers))
            (writer
             (values '(new-value object) writer-specializers
                     'global-writer-method
                     (make-std-writer-method-function 'slot-object slot-name)
                     "automatically-generated writer method"
                     (make-fallback-writer-method-function slot-name)
                     fallback-writer-specializers))
            (boundp
             (values '(object) reader-specializers 'global-boundp-method
                     (make-std-boundp-method-function 'slot-object slot-name)
                     "automatically-generated boundp method"
                     (make-fallback-boundp-method-function slot-name)
                     fallback-reader-specializers))
            (makunbound
             (values '(object) reader-specializers 'global-makunbound-method
                     (make-std-makunbound-method-function 'slot-object slot-name)
                     "automatically-generated makunbound method"
                     (make-fallback-makunbound-method-function slot-name)
                     fallback-reader-specializers)))
        (let ((gf (ensure-generic-function fun-name :lambda-list lambda-list)))
          (add-method gf (make-a-method method-class ()
                                        lambda-list fallback-specializers
                                        fallback-initargs doc :slot-name slot-name))
          (add-method gf (make-a-method method-class ()
                                        lambda-list specializers
                                        initargs doc :slot-name slot-name)))))))
#+sb-xc-host (setf (fdefinition 'ensure-accessor) #'real-ensure-accessor)

;;; PCL proper's classes, generic functions and methods so far, on the host
;;; (where the target's PCL, booted, would fix its early generic functions),
;;; and what is made of them below, but for genesis, which make-host-2's
;;; writes (SB-COLD::*BUILD-HOST-PCL*)
#+sb-xc-host (when sb-cold::*build-host-pcl* (host-pcl-build-proper))

#+sb-xc-host
(when *host-pcl-ready*
(dolist (gf-name *!temporary-ensure-accessor-functions*)
  ; (format t "~&Genericizing ~S~%" gf-name)
  (fmakunbound gf-name)
  (ensure-accessor gf-name)))

#+sb-xc-host (when *host-pcl-ready* (compute-standard-slot-locations))

;;; Once PCL's bootstrap is over (!FINISH-GENESIS-BRAID): PCL makes the
;;; classes of the conditions and structures defined from then on, and
;;; finishes those genesis wrote
#-sb-xc-host
(defun !hook-pcl-into-system ()
(pushnew 'ensure-deffoo-class sb-kernel::*define-condition-hooks*) ; braid.lisp
(setq sb-kernel::*defstruct-hooks* '(ensure-defstruct-class))
(dolist (s '(condition function structure-object))
  (sb-kernel::do-subclassoids ((k v) (find-classoid s))
    (declare (ignore v))
    (if (and (condition-classoid-p k) (classoid-pcl-class k))
        ;; Genesis wrote the class, but a condition's slots are only known
        ;; once its DEFINE-CONDITION has run in cold init. Not
        ;; REINITIALIZE-INSTANCE: its :AFTER method would make reader
        ;; methods before late-condition.lisp can.
        (let ((class (classoid-pcl-class k)))
          (shared-initialize class nil
                             :direct-superclasses (class-direct-superclasses class)
                             :direct-slots (condition-class-direct-slot-initargs k)))
        (let ((class (find-class (classoid-name k))))
          ;; Genesis wrote a structure's class with the plain vector mapper;
          ;; big ones get their lazily compiled one now, as when PCL made them.
          (when (structure-class-p class)
            (sb-kernel::install-struct-slot-mapper (classoid-layout k))))))))

;;; CLASS-PROTOTYPE for FUNCTION should not use ALLOCATE-INSTANCE.
;;;
#+sb-xc-host
(when *host-pcl-ready*
(let ((class (find-class 'function)))
  (setf (slot-value class 'prototype) #'identity)))

;;; The standard and OR method combinations (defcombin.lisp): genesis
;;; writes their info, which host PCL makes
#+sb-xc-host
(when *host-pcl-ready*
(setf (gethash 'standard (method-combinations))
      (make-method-combination-info
       :constructor (standard-method-combination-constructor)
       :cache (list (cons nil *standard-method-combination*))))
(let ((info (gethash 'or (method-combinations))))
  (aver info)
  (aver (null (method-combination-info-cache info)))
  (setf (method-combination-info-cache info)
        (list (cons '(:most-specific-first) *or-method-combination*))))
(add-to-weak-hashset #'make-specializer-form-using-class
                     (method-combination-gf-set *or-method-combination*)))

;;; (methods.lisp)
#+sb-xc-host
(when *host-pcl-ready*
(mapc
 #'!proclaim-incompatible-superclasses
 '(;; superclass class
   (system-class std-class structure-class) ; direct subclasses of pcl-class
   (standard-class funcallable-standard-class)
   ;; superclass metaobject
   (class eql-specializer class-eq-specializer method method-combination
    generic-function slot-definition)
   ;; metaclass built-in-class
   (number sequence character           ; direct subclasses of t, but not array
    standard-object structure-object)   ;                        or symbol
   (number array character symbol       ; direct subclasses of t, but not
    standard-object structure-object)   ;                        sequence
   (complex float rational)             ; direct subclasses of number
   (integer ratio)                      ; direct subclasses of rational
   (list vector)                        ; direct subclasses of sequence
   (cons null)                          ; direct subclasses of list
   (string bit-vector)                  ; direct subclasses of vector
   )))

;;; consistency check: all our standard SLOT-VALUE-USING-CLASS methods are available
;;; and distinct from each other.
#+sb-xc-host
(when *host-pcl-ready*
(let (set)
  (dolist (type '(reader writer boundp makunbound))
    (dolist (fun '(standard-svuc-method structure-svuc-method condition-svuc-method))
      (pushnew (funcall fun type) set)))
  (assert (= (length set) 12))))

#-sb-xc-host
(dolist (symbol '(add-method allocate-instance class-name compute-applicable-methods
                  ensure-generic-function make-instance method-qualifiers
                  remove-method add-dependent add-direct-method add-direct-subclass
                  class-default-initargs class-direct-default-initargs
                  class-direct-slots class-direct-subclasses
                  class-direct-superclasses class-finalized-p class-precedence-list
                  class-prototype class-slots
                  compute-applicable-methods-using-classes
                  compute-class-precedence-list compute-default-initargs
                  compute-discriminating-function compute-effective-method
                  compute-effective-slot-definition compute-slots
                  direct-slot-definition direct-slot-definition-class
                  effective-slot-definition effective-slot-definition-class
                  ensure-class ensure-class-using-class
                  ensure-generic-function-using-class eql-specializer
                  eql-specializer-object extract-lambda-list
                  extract-specializer-names finalize-inheritance
                  find-method-combination forward-referenced-class
                  funcallable-standard-class funcallable-standard-instance-access
                  funcallable-standard-object
                  generic-function-argument-precedence-order
                  generic-function-declarations generic-function-lambda-list
                  generic-function-method-class generic-function-method-combination
                  generic-function-methods generic-function-name
                  intern-eql-specializer make-method-lambda map-dependents
                  method-function method-generic-function method-lambda-list
                  method-specializers accessor-method-slot-definition
                  reader-method-class remove-dependent remove-direct-method
                  remove-direct-subclass set-funcallable-instance-function
                  slot-boundp-using-class slot-definition slot-definition-allocation
                  slot-definition-initargs slot-definition-initform
                  slot-definition-initfunction slot-definition-location
                  slot-definition-name slot-definition-readers
                  slot-definition-writers slot-definition-type
                  slot-makunbound-using-class slot-value-using-class specializer
                  specializer-direct-generic-functions specializer-direct-methods
                  standard-accessor-method standard-direct-slot-definition
                  standard-effective-slot-definition standard-instance-access
                  standard-reader-method standard-slot-definition
                  standard-writer-method update-dependent validate-superclass
                  writer-method-class))
  (sb-impl::deprecate-export *package* symbol :late "2.0.7"))

(in-package "SB-KERNEL")
(defun slot-object-p (x) (typep x '(or structure-object standard-object condition)))

#-sb-xc-host
(flet ((set-predicate (classoid-name pred)
         (let ((c (find-classoid classoid-name)))
           (%instance-set c (get-dsd-index built-in-classoid predicate)
                 pred))))
  (set-predicate 't #'constantly-t)
  (set-predicate 'random-class #'constantly-nil)
  (set-predicate 'sb-pcl::slot-object #'slot-object-p))
