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

(in-package "SB!PCL")

#|

The CommonLoops evaluator is meta-circular.

Most of the code in PCL is methods on generic functions, including
most of the code that actually implements generic functions and method
lookup.

So, we have a classic bootstrapping problem. The solution to this is
to first get a cheap implementation of generic functions running,
these are called early generic functions. These early generic
functions and the corresponding early methods and early method lookup
are used to get enough of the system running that it is possible to
create real generic functions and methods and implement real method
lookup. At that point (done in the file FIXUP) the function
!FIX-EARLY-GENERIC-FUNCTIONS is called to convert all the early generic
functions to real generic functions.

The cheap generic functions are built using the same
FUNCALLABLE-INSTANCE objects that real generic functions are made out of.
This means that as PCL is being bootstrapped, the cheap generic
function objects which are being created are the same objects which
will later be real generic functions. This is good because:
  - we don't cons garbage structure, and
  - we can keep pointers to the cheap generic function objects
    during booting because those pointers will still point to
    the right object after the generic functions are all fixed up.

This file defines the DEFMETHOD macro and the mechanism used to expand
it. This includes the mechanism for processing the body of a method.
DEFMETHOD basically expands into a call to LOAD-DEFMETHOD, which
basically calls ADD-METHOD to add the method to the generic function.
These expansions can be loaded either during bootstrapping or when PCL
is fully up and running.

An important effect of this arrangement is it means we can compile
files with DEFMETHOD forms in them in a completely running PCL, but
then load those files back in during bootstrapping. This makes
development easier. It also means there is only one set of code for
processing DEFMETHOD. Bootstrapping works by being sure to have
LOAD-METHOD be careful to call only primitives which work during
bootstrapping.

|#

(declaim (notinline make-a-method add-named-method
                    ensure-generic-function-using-class
                    add-method remove-method))

(defvar *!early-functions*
  '((make-a-method !early-make-a-method real-make-a-method)
    (add-named-method !early-add-named-method real-add-named-method)))

;;; For each of the early functions, arrange to have it point to its
;;; early definition. Do this in a way that makes sure that if we
;;; redefine one of the early definitions the redefinition will take
;;; effect. This makes development easier.
#+sb-xc
(loop for (name early-name) in *!early-functions*
   do (let ((early-name early-name))
        (setf (gdefinition name)
              (set-fun-name
               (lambda (&rest args)
                 (apply (fdefinition early-name) args))
               name))))

;;; *!GENERIC-FUNCTION-FIXUPS* is used by !FIX-EARLY-GENERIC-FUNCTIONS
;;; to convert the few functions in the bootstrap which are supposed
;;; to be generic functions but can't be early on.
;;;
;;; each entry is a list of the form
;;;
;;;   (GENERIC-FUNCTION-NAME METHOD-COMBINATION-NAME METHODS)
;;;
;;; where methods is a list of lists of the form
;;;
;;;   (LAMBDA-LIST SPECIALIZERS QUALIFIERS METHOD-BODY-FUNCTION-NAME)
;;;
;;;,where SPECIALIZERS is a list of class names.
(defvar *!generic-function-fixups*
  '((add-method
     standard
     ((generic-function method)
      (standard-generic-function method)
      ()
      real-add-method))

    (remove-method
     standard
     ((generic-function method)
      (standard-generic-function method)
      ()
      real-remove-method))

    (get-method
     standard
     ((generic-function qualifiers specializers &optional (errorp t))
      (standard-generic-function t t)
      ()
      real-get-method))

    (ensure-generic-function-using-class
     standard
     ((generic-function fun-name
                        &key generic-function-class environment
                        &allow-other-keys)
      (generic-function t)
      ()
      real-ensure-gf-using-class--generic-function)
     ((generic-function fun-name
                        &key generic-function-class environment
                        &allow-other-keys)
      (null t)
      ()
      real-ensure-gf-using-class--null))

    (make-method-lambda
     standard
     ((proto-generic-function proto-method lambda-expression environment)
      (standard-generic-function standard-method t t)
      ()
      real-make-method-lambda))

    (make-method-lambda-using-specializers
     standard
     ((proto-generic-function proto-method qualifiers specializers
                              lambda-expression environment)
      (standard-generic-function standard-method t t t t)
      ()
      real-make-method-lambda-using-specializers))

    (make-method-specializers-form
     standard
     ((proto-generic-function proto-method specializer-names environment)
      (standard-generic-function standard-method t t)
      ()
      real-make-method-specializers-form))

    (make-specializer-form-using-class
     or
     ((proto-generic-function proto-method specializer-name environment)
      (standard-generic-function standard-method t t)
      (or)
      real-make-specializer-form-using-class/t)
     ((proto-generic-function proto-method specializer-name environment)
      (standard-generic-function standard-method specializer t)
      (or)
      real-make-specializer-form-using-class/specializer)
     ((proto-generic-function proto-method specializer-name environment)
      (standard-generic-function standard-method symbol t)
      (or)
      real-make-specializer-form-using-class/symbol)
     ((proto-generic-function proto-method specializer-name environment)
      (standard-generic-function standard-method cons t)
      (or)
      real-make-specializer-form-using-class/cons))

    (specializer-type-specifier
     standard
     ((proto-generic-function proto-method specializer)
      (standard-generic-function standard-method specializer)
      ()
      real-specializer-type-specifier/specializer)
     ((proto-generic-function proto-method specializer)
      (standard-generic-function standard-method symbol)
      ()
      real-specializer-type-specifier/symbol)
     ((proto-generic-function proto-method specializer)
      (standard-generic-function standard-method t)
      ()
      real-specializer-type-specifier/t)
     ((proto-generic-function proto-method specializer)
      (standard-generic-function standard-method class-eq-specializer)
      ()
      real-specializer-type-specifier/class-eq-specializer)
     ((proto-generic-function proto-method specializer)
      (standard-generic-function standard-method eql-specializer)
      ()
      real-specializer-type-specifier/eql-specializer)
     ((proto-generic-function proto-method specializer)
      (standard-generic-function standard-method structure-class)
      ()
      real-specializer-type-specifier/structure-class)
     ((proto-generic-function proto-method specializer)
      (standard-generic-function standard-method system-class)
      ()
      real-specializer-type-specifier/system-class)
     ((proto-generic-function proto-method specializer)
      (standard-generic-function standard-method class)
      ()
      real-specializer-type-specifier/class))

    (parse-specializer-using-class
     standard
     ((generic-function specializer)
      (standard-generic-function t)
      ()
      real-parse-specializer-using-class))

    (unparse-specializer-using-class
     standard
     ((generic-function specializer)
      (standard-generic-function t)
      ()
      real-unparse-specializer-using-class))

    (make-method-initargs-form
     standard
     ((proto-generic-function proto-method
                              lambda-expression
                              lambda-list environment)
      (standard-generic-function standard-method t t t)
      ()
      real-make-method-initargs-form))

    (compute-effective-method
     standard
     ((generic-function combin applicable-methods)
      (generic-function standard-method-combination t)
      ()
      standard-compute-effective-method)
     ((generic-function combin applicable-methods)
      (generic-function short-method-combination t)
      ()
      short-compute-effective-method))))


(defun !fix-early-generic-functions ()
  (let ((accessors nil))
    ;; Rearrange *!EARLY-GENERIC-FUNCTIONS* to speed up
    ;; FIX-EARLY-GENERIC-FUNCTIONS.
    (dolist (early-gf-spec *!early-generic-functions*)
      (when (every #'early-method-standard-accessor-p
                   (early-gf-methods (gdefinition early-gf-spec)))
        (push early-gf-spec accessors)))
    (dolist (spec (nconc accessors
                         '(accessor-method-slot-name
                           generic-function-methods
                           method-specializers
                           specializer-type
                           specializer-class
                           slot-definition-location
                           slot-definition-name
                           class-slots
                           gf-arg-info
                           class-precedence-list
                           slot-boundp-using-class
                           (setf slot-value-using-class)
                           slot-value-using-class)))
      (/show spec)
      (setq *!early-generic-functions*
            (cons spec
                  (delete spec *!early-generic-functions* :test #'equal))))

    (dolist (early-gf-spec *!early-generic-functions*)
      (/show early-gf-spec)
      (let* ((gf (gdefinition early-gf-spec))
             (methods (mapcar (lambda (early-method)
                                (let ((args (copy-list (fifth
                                                        early-method))))
                                  (setf (fourth args)
                                        (early-method-specializers
                                         early-method t))
                                  (apply #'real-make-a-method args)))
                              (early-gf-methods gf))))
        (setf (generic-function-method-class gf) *the-class-standard-method*)
        (setf (generic-function-method-combination gf)
              *standard-method-combination*)
        (set-methods gf methods)))

    (dolist (fn *!early-functions*)
      (/show fn)
      (setf (gdefinition (car fn)) (fdefinition (caddr fn))))

    (loop for (fspec method-combination . methods) in *!generic-function-fixups*
          for gf = (gdefinition fspec) do
          (labels ((translate-source-location (function)
                     ;; This is lifted from sb!introspect, OAOO and all that.
                     (let* ((function-object (sb!kernel::%fun-fun function))
                            (function-header (sb!kernel:fun-code-header function-object))
                            (debug-info (sb!kernel:%code-debug-info function-header))
                            (debug-source (sb!c::debug-info-source debug-info))
                            (debug-fun (debug-info-debug-function function debug-info)))
                       (sb!c::%make-definition-source-location
                        (sb!c::debug-source-namestring debug-source)
                        (sb!c::compiled-debug-info-tlf-number debug-info)
                        (sb!c::compiled-debug-fun-form-number debug-fun))))
                   (debug-info-debug-function (function debug-info)
                     (let ((map (sb!c::compiled-debug-info-fun-map debug-info))
                           (name (sb!kernel:%simple-fun-name (sb!kernel:%fun-fun function))))
                       (or
                        (find-if
                         (lambda (x)
                           (and
                            (sb!c::compiled-debug-fun-p x)
                            (eq (sb!c::compiled-debug-fun-name x) name)))
                         map)
                        (elt map 0))))
                   (make-method (spec)
                     (destructuring-bind
                         (lambda-list specializers qualifiers fun-name) spec
                       (let* ((specializers (mapcar #'sb-xc:find-class specializers))
                              (fun-name (or fun-name fspec))
                              (fun (fdefinition fun-name))
                              (initargs (list :function
                                              (set-fun-name
                                               (early-gf-primary-slow-method-fn fun)
                                               `(call ,fun-name)))))
                         (declare (type function fun))
                         (make-a-method
                          'standard-method
                          qualifiers lambda-list specializers initargs nil
                          :definition-source (translate-source-location fun))))))
            (setf (generic-function-method-class gf)
                  *the-class-standard-method*
                  (generic-function-method-combination gf)
                  (ecase method-combination
                    (standard *standard-method-combination*)
                    (or *or-method-combination*)))
            (set-methods gf (mapcar #'make-method methods)))))

  (/show "leaving !FIX-EARLY-GENERIC-FUNCTIONS"))


;;; FIXME: In here there was a #!-CMU definition of SYMBOL-MACROLET
;;; which used %WALKER stuff. That suggests to me that maybe the code
;;; walker stuff was only used for implementing stuff like that; maybe
;;; it's not needed any more? Hunt down what it was used for and see.

(defun extract-the (form)
  (cond ((and (consp form) (eq (car form) 'the))
         (aver (proper-list-of-length-p form 3))
         (third form))
        (t
         form)))

(setq **boot-state** 'early)

(sb-xc:defmacro with-slots (slots instance &body body)
  (let ((in (gensym)))
    `(let ((,in ,instance))
       (declare (ignorable ,in))
       ,@(let ((instance (extract-the instance)))
           (and (symbolp instance)
                `((declare (%variable-rebinding ,in ,instance)))))
       ,in
       (symbol-macrolet ,(mapcar (lambda (slot-entry)
                                   (let ((var-name
                                          (if (symbolp slot-entry)
                                              slot-entry
                                              (car slot-entry)))
                                         (slot-name
                                          (if (symbolp slot-entry)
                                              slot-entry
                                              (cadr slot-entry))))
                                     `(,var-name
                                       (slot-value ,in ',slot-name))))
                                 slots)
                        ,@body))))

(sb-xc:defmacro with-accessors (slots instance &body body)
  (let ((in (gensym)))
    `(let ((,in ,instance))
       (declare (ignorable ,in))
       ,@(let ((instance (extract-the instance)))
           (and (symbolp instance)
                `((declare (%variable-rebinding ,in ,instance)))))
       ,in
       (symbol-macrolet ,(mapcar (lambda (slot-entry)
                                   (let ((var-name (car slot-entry))
                                         (accessor-name (cadr slot-entry)))
                                     `(,var-name (,accessor-name ,in))))
                                 slots)
          ,@body))))
