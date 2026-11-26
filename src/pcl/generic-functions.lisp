;;;; Mostly this file contains generic functions. The exceptions are hacks.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is in the public domain and is provided with absolutely no
;;;; warranty. See the COPYING and CREDITS files for more information.

(in-package "SB-PCL")

;;;; readers

(def!generic accessor-method-slot-definition (standard-accessor-method))

(def!generic class-can-precede-list (pcl-class))

(def!generic class-defstruct-constructor (structure-class))

(def!generic class-defstruct-form (structure-class))

(def!generic class-direct-subclasses (class))

(def!generic class-direct-superclasses (class))

(def!generic class-eq-specializer (class))

(def!generic class-incompatible-superclass-list (pcl-class))

(def!generic class-name (class))

(def!generic class-precedence-list (pcl-class))

(def!generic class-wrapper (pcl-class))

(def!generic definition-source (definition-source-mixin))

(def!generic eql-specializer-object (eql-specializer))

(def!generic generic-function-declarations (generic-function))

(def!generic generic-function-method-class (generic-function))

(def!generic generic-function-method-combination (generic-function))

(def!generic generic-function-methods (generic-function))

(def!generic generic-function-name (generic-function))

(def!generic gf-arg-info (standard-generic-function))

(def!generic gf-dfun-state (standard-generic-function))

(def!generic generic-function-initial-methods (standard-generic-function))

(def!generic long-method-combination-function (long-method-combination))

(def!generic method-combination-documentation (standard-method-combination))

(def!generic method-combination-options (standard-method-combination))

(def!generic method-combination-type-name (standard-method-combination))

(def!generic method-generic-function (standard-method))

(def!generic object-plist (plist-mixin))

(def!generic short-combination-identity-with-one-argument
  (short-method-combination))

(def!generic short-combination-operator (short-method-combination))

(def!generic slot-definition-class (slot-definition))

(def!generic slot-definition-defstruct-accessor-symbol
  (structure-slot-definition))

(def!generic slot-definition-initargs (slot-definition))

(def!generic slot-definition-initform (slot-definition))

(def!generic slot-definition-initfunction (slot-definition))

(def!generic slot-definition-internal-reader-function
  (structure-slot-definition))

(def!generic slot-definition-internal-writer-function
  (structure-slot-definition))

(def!generic slot-definition-location (standard-effective-slot-definition))

(def!generic slot-definition-name (slot-definition))

(def!generic slot-definition-info (effective-slot-definition))

(def!generic slot-definition-readers (slot-definition))

(def!generic slot-definition-type (slot-definition))

(def!generic slot-definition-writers (slot-definition))

(def!generic specializer-object (class-eq-specializer))

(def!generic specializer-type (specializer))

;;;; writers

(def!generic (setf class-defstruct-constructor) (new-value structure-class))

(def!generic (setf class-defstruct-form) (new-value structure-class))

(def!generic (setf class-direct-slots) (new-value slot-class))

(def!generic (setf class-incompatible-superclass-list) (new-value pcl-class))

(def!generic (setf class-name) (new-value class))

(def!generic (setf class-slots) (new-value slot-class))

(def!generic (setf generic-function-method-class) (new-value
                                                  standard-generic-function))

(def!generic (setf generic-function-method-combination)
  (new-value standard-generic-function))

(def!generic (setf generic-function-declarations) (new-value
                                                  standard-generic-function))

(def!generic (setf generic-function-methods) (new-value
                                             standard-generic-function))

(def!generic (setf generic-function-name) (new-value standard-generic-function))

(def!generic (setf gf-dfun-state) (new-value standard-generic-function))

(def!generic (setf generic-function-initial-methods)
  (new-value standard-generic-function))

(def!generic (setf method-generic-function) (new-value standard-method))

(def!generic (setf object-plist) (new-value plist-mixin))

(def!generic (setf slot-definition-allocation) (new-value
                                               standard-slot-definition))

(def!generic (setf slot-definition-class) (new-value slot-definition))

(def!generic (setf slot-definition-defstruct-accessor-symbol)
  (new-value structure-slot-definition))

(def!generic (setf slot-definition-initargs) (new-value slot-definition))

(def!generic (setf slot-definition-initform) (new-value slot-definition))

(def!generic (setf slot-definition-initfunction) (new-value slot-definition))

(def!generic (setf slot-definition-internal-reader-function)
  (new-value structure-slot-definition))

(def!generic (setf slot-definition-internal-writer-function)
  (new-value structure-slot-definition))

(def!generic (setf slot-definition-location)
  (new-value standard-effective-slot-definition))

(def!generic (setf slot-definition-name) (new-value slot-definition))

(def!generic (setf slot-definition-info) (new-value effective-slot-definition))

(def!generic (setf slot-definition-readers) (new-value slot-definition))

(def!generic (setf slot-definition-type) (new-value slot-definition))

(def!generic (setf slot-definition-writer-function)
  (new-value effective-slot-definition))

(def!generic (setf slot-definition-writers) (new-value slot-definition))

;;;; 1 argument

(def!generic accessor-method-slot-name (m))

(def!generic class-default-initargs (class))

(def!generic class-direct-default-initargs (class))

(def!generic class-direct-slots (class))

(def!generic class-finalized-p (class))

(def!generic class-prototype (class))

(def!generic class-slot-cells (class))

(def!generic class-slots (class))

(def!generic compute-class-precedence-list (root))

(def!generic compute-default-initargs (class))

(def!generic compute-discriminating-function (gf))

(def!generic compute-discriminating-function-arglist-info (generic-function))

(def!generic compute-slots (class))

(def!generic finalize-inheritance (class))

(def!generic function-keywords (method))

(def!generic generic-function-argument-precedence-order (generic-function))

(def!generic generic-function-lambda-list (generic-function))

(def!generic generic-function-pretty-arglist (generic-function &optional current-defmethod))

(def!generic gf-fast-method-function-p (gf))

(def!generic initialize-internal-slot-functions (slotd))

(def!generic make-instances-obsolete (class))

(def!generic method-function (method))

(def!generic method-lambda-list (m))

(def!generic method-qualifiers (m))

(def!generic method-specializers (m))

(def!generic slot-definition-allocation (slotd))

(def!generic specializer-class (specializer))

(def!generic specializer-direct-generic-functions (specializer))

(def!generic specializer-direct-methods (specializer))

(def!generic specializer-method-table (specializer))

(def!generic specializer-method-holder (specializer &optional create))

(def!generic update-constructors (class))

;;;; 2 arguments

(def!generic add-dependent (metaobject dependent))

(def!generic add-direct-method (specializer method))

(def!generic add-direct-subclass (class subclass))

(def!generic add-method (generic-function method))

(def!generic (setf class-slot-cells) (new-value class))

(def!generic class-slot-value (class slot-name))

(def!generic compatible-meta-class-change-p (class proto-new-class))

(def!generic compute-applicable-methods (generic-function arguments))

(def!generic compute-applicable-methods-using-classes
  (generic-function classes))

(def!generic compute-effective-slot-definition-initargs (class direct-slotds))

(def!generic describe-object (object stream))

(def!generic direct-slot-definition-class (class &rest initargs))

(def!generic effective-slot-definition-class (class &rest initargs))

(def!generic make-reader-method-function (class slot-name))

(def!generic make-writer-method-function (class slot-name))

(def!generic map-dependents (metaobject function))

(def!generic parse-specializer-using-class (generic-function specializer-name)
  (:documentation
   "Parse SPECIALIZER-NAME into a specializer object suitable for GENERIC-FUNCTION.

If SPECIALIZER-NAME is not well-formed with respect to the specializer
syntax of GENERIC-FUNCTION, an error of type
SB-PCL:SPECIALIZER-NAME-SYNTAX-ERROR is signaled.

If GENERIC-FUNCTION is a STANDARD-GENERIC-FUNCTION and
SPECIALIZER-NAME is a symbol that does not name an existing class, an
error of type SB-PCL:CLASS-NOT-FOUND-ERROR is signaled.

Other errors may be signaled for generic function classes other than
STANDARD-GENERIC-FUNCTION.

If GENERIC-FUNCTION is a STANDARD-GENERIC-FUNCTION and
SPECIALIZER-NAME is of the form (eql OBJECT), OBJECT is not
evaluated (in contrast to DEFMETHOD's behavior).

NOTE: This generic function is part of an SBCL-specific experimental
protocol. Interface subject to change."))

(def!generic remove-dependent (metaobject dependent))

(def!generic remove-direct-method (specializer method))

(def!generic remove-direct-subclass (class subclass))

(def!generic remove-method (generic-function method))

(def!generic remove-reader-method (class generic-function))

(def!generic remove-writer-method (class generic-function))

(def!generic same-specializer-p (specl1 specl2))

(def!generic slot-accessor-function (slotd type))

(def!generic slot-accessor-std-p (slotd type))

;;; This controls DESCRIBE-OBJECT (SLOT-OBJECT STREAM) behavior.
(def!generic slots-to-inspect (class object))

(def!generic unparse-specializer-using-class (generic-function specializer))

(def!generic validate-superclass (class superclass))

(def!generic invalid-superclass (class superclass))


;;;; 3 arguments

(def!generic (setf class-slot-value) (nv class slot-name))

;;; CMUCL comment (from Gerd Moellmann/Pierre Mai, 2002-10-19):
;;;
;;; According to AMOP, COMPUTE-EFFECTIVE-METHOD should return two
;;; values.  Alas, the second value is only vaguely described in AMOP,
;;; and, when asked on 2002-10-18, Gregor Kiczales said he couldn't
;;; remember what the second value was supposed to be.  So, PCL's
;;; COMPUTE-EFFECTIVE-METHOD returns one value as do Allegro and
;;; Lispworks.
(def!generic compute-effective-method
    (generic-function combin applicable-methods))

(def!generic compute-primary-methods
    (generic-function combin applicable-methods))

(def!generic compute-effective-slot-definition (class name dslotds))

(def!generic compute-slot-accessor-info (slotd type gf))

(def!generic find-method-combination (generic-function type options))

(def!generic invalid-qualifiers (generic-function combin method))

(def!generic (setf slot-accessor-function) (function slotd type))

(def!generic (setf slot-accessor-std-p) (value slotd type))

(def!generic slot-boundp-using-class (class object slotd))

(def!generic slot-makunbound-using-class (class object slotd))

(def!generic slot-unbound (class instance slot-name))

(def!generic slot-value-using-class (class object slotd))

(def!generic specializer-type-specifier
    (proto-generic-function proto-method specializer)
  (:documentation
   "Return a type specifier for SPECIALIZER, a non-parsed specializer
form or a SPECIALIZER instance.

More specifically, SPECIALIZER can be
* a non-parsed specializer form such as

    * a symbol naming a class
    * a list of the form (eql OBJECT)
    * a list of the form `(SPECIALIZER-KIND &REST SPECIFIC-SYNTAX)`

* an instance of a subclass of SPECIALIZER

When SPECIALIZER cannot be parsed/used as a specializer for
PROTO-GENERIC-FUNCTION and PROTO-METHOD, a STYLE-WARNING is signaled
and NIL is returned. No type declaration will be generated in this
case.

NIL can also be returned if SPECIALIZER is valid but its type should
not be declared, for example for efficiency reasons.

NOTE: This generic function is part of an SBCL-specific experimental
protocol. Interface subject to change."))

;;;; 4 arguments

(def!generic make-method-lambda
    (proto-generic-function proto-method lambda-expression environment))

(def!generic make-method-specializers-form
    (proto-generic-function proto-method specializer-names environment))

;;; MAKE-SPECIALIZER-FORM-USING-CLASS
;;;
;;; To free every new custom generic function class from having to
;;; implement iteration over specializers in
;;; MAKE-METHOD-SPECIALIZERS-FORM, we provide a default method
;;;
;;;   make-method-specializers-form standard-g-f standard-method
;;;
;;; which performs this iteration and calls the generic function
;;;
;;;   make-specializer-form-using-class proto-g-f proto-m specializer-name env
;;;
;;; on which custom generic function classes can install methods to
;;; handle their custom specializers. The generic function uses OR
;;; method combination to allow the following idiom:
;;;
;;;   (defmethod make-specializer-form-using-class or
;;;       (proto-generic-function MY-GENERIC-FUNCTION)
;;;       (proto-method standard-method)
;;;       (specializer-name cons)
;;;       (environment t))
;;;     (when (typep specializer-name '(cons (eql MY-SPECIALIZER)))
;;;       MY-SPECIALIZER-FORM))
;;;
;;; The OR method combination lets everything but (my-specializer ...)
;;; fall through to the next methods which will, at some point, handle
;;; class and eql specializers and eventually reach an error signaling
;;; method for invalid specializers.

(def!generic make-specializer-form-using-class
    (proto-generic-function proto-method specializer-name environment)
  (:method-combination or)
  (:documentation
   "Return a form which, when evaluated in the lexical environment
described by ENVIRONMENT, parses the specializer SPECIALIZER-NAME and
yields the appropriate specializer object.

Both PROTO-GENERIC-FUNCTION and PROTO-METHOD may be
uninitialized. However their classes and prototypes can be
inspected.

NOTE: This generic function is part of an SBCL-specific experimental
protocol. Interface subject to change."))

(def!generic (setf slot-value-using-class) (new-value class object slotd))

;;;; 5 arguments

(def!generic add-reader-method (class generic-function slot-name slot-documentation source-location))

(def!generic add-writer-method (class generic-function slot-name slot-documentation source-location))

(def!generic make-method-initargs-form
    (proto-generic-function proto-method lambda-expression lambda-list
     environment))

;;;; 6 arguments

(def!generic make-method-lambda-using-specializers
    (proto-generic-function proto-method qualifiers specializers
     method-lambda environment)
  (:documentation
   "Compute a method lambda form based on METHOD-LAMBDA, possibly
taking into account PROTO-GENERIC-FUNCTION, PROTO-METHOD, QUALIFIERS,
SPECIALIZERS and ENVIRONMENT.

Both PROTO-GENERIC-FUNCTION and PROTO-METHOD may be
uninitialized. However, their classes and prototypes can be inspected.

SPECIALIZERS is a list of specializer objects (i.e. parsed).

Return three values:
1. the created method lambda form
2. initargs for the method instance
3. a (possibly modified) unspecialized method lambda list or nil if
   the unspecialized lambda list contained in METHOD-LAMBDA should be
   used

NOTE: This generic function is part of an SBCL-specific experimental
protocol. Interface subject to change."))

;;;; optional arguments

(def!generic get-method (generic-function
                        qualifiers
                        specializers
                        &optional errorp))

(def!generic find-method (generic-function
                         qualifiers
                         specializers
                         &optional errorp))

(def!generic slot-missing (class
                          instance
                          slot-name
                          operation
                          &optional new-value))

;;;; &KEY arguments

;;; FIXME: make the declared &KEY arguments here agree with those that
;;; AMOP specifies.
(def!generic allocate-instance (class &rest initargs))

(def!generic ensure-class-using-class (class
                                      name
                                      &rest args
                                      &key &allow-other-keys))

(def!generic ensure-generic-function-using-class (generic-function
                                                 fun-name
                                                 &key &allow-other-keys))

(def!generic initialize-instance (instance &rest initargs &key &allow-other-keys))

(def!generic make-instance (class &rest initargs &key &allow-other-keys))

(def!generic change-class (instance new-class-name &rest initargs &key &allow-other-keys))

(def!generic no-applicable-method (generic-function &rest args))

(def!generic no-next-method (generic-function method &rest args))

(def!generic no-primary-method (generic-function &rest args))

(def!generic reader-method-class (class direct-slot &rest initargs))

(def!generic reinitialize-instance (instance &rest initargs &key &allow-other-keys))

(def!generic shared-initialize (instance slot-names &rest initargs
                               &key &allow-other-keys))

(def!generic update-dependent (metaobject dependent &rest initargs))

(def!generic update-instance-for-different-class (previous
                                                 current
                                                 &rest initargs))

(def!generic update-instance-for-redefined-class (instance
                                                 added-slots
                                                 discarded-slots
                                                 property-list
                                                 &rest initargs))

(def!generic writer-method-class (class direct-slot &rest initargs))
