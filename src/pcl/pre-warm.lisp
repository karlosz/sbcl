;;;; In support of PCL we compile some things into the cold image.
;;;; Not only does this simplify the PCL bootstrap ever so slightly,
;;;; it is nice to be able to test for type CLASS

;;;; This software is part of the SBCL system. See the README file for more
;;;; information.

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

;;; Set up fake standard-classes.
;;; This is enough to fool the compiler into optimizing TYPEP into
;;; %INSTANCE-TYPEP.
;;; I'll bet that at least half of these we don't need at all.
(defparameter *!early-class-predicates*
  '((specializer specializerp)
    (standard-specializer standard-specializer-p)
    (exact-class-specializer exact-class-specializer-p)
    (class-eq-specializer class-eq-specializer-p)
    (eql-specializer eql-specializer-p)
    (system-class system-class-p)
    (class classp)
    (slot-class slot-class-p)
    (std-class std-class-p)
    (standard-class standard-class-p)
    (funcallable-standard-class funcallable-standard-class-p)
    (condition-class condition-class-p)
    (structure-class structure-class-p)
    (forward-referenced-class forward-referenced-class-p)
    (method method-p) ; shouldn't this be spelled METHODP? (like CLASSP)
    (standard-method standard-method-p)
    (accessor-method accessor-method-p)
    (standard-accessor-method standard-accessor-method-p)
    (standard-reader-method standard-reader-method-p)
    (standard-writer-method standard-writer-method-p)
    (standard-boundp-method standard-boundp-method-p)
    (global-reader-method global-reader-method-p)
    (global-writer-method global-writer-method-p)
    (global-boundp-method global-boundp-method-p)
    (generic-function generic-function-p)
    (standard-generic-function standard-generic-function-p)
    (method-combination method-combination-p)
    (long-method-combination long-method-combination-p)
    (short-method-combination short-method-combination-p)))

#+sb-xc-host
(flet ((create-fake-classoid (name fun-p)
         (let* ((classoid (make-standard-classoid :name name))
                (cell (sb!kernel::make-classoid-cell name classoid))
                (layout
                 (make-layout
                  :classoid classoid
                  :inherits (map 'vector #'find-layout
                                 (cons t (if fun-p '(function))))
                  :length 0 ; don't care
                  :depthoid -1
                  :invalid nil)))
           (setf (classoid-layout classoid) layout
                 (info :type :classoid-cell name) cell
                 (info :type :kind name) :instance))))
  ;; Because we don't wire into %INSTANCE-TYPEP any assumptions about
  ;; the superclass/subclass relationships, these can all trivially be faked.
  (dolist (x *!early-class-predicates*)
    (let ((name (car x)))
      ;; GENERIC-FUNCTION and STANDARD-GENERIC-FUNCTION must contain
      ;; FUNCTION in their layouts so that their type predicates
      ;; optimize into FUNCALLABLE-INSTANCE-P (followed by a layout check),
      ;; rather than testing both that and INSTANCEP.
      (create-fake-classoid name
                            (memq name '(standard-generic-function
                                         generic-function))))))

;;; BIG FAT WARNING: These predicates can't in general be called prior to the
;;; definition of the class which they test. However in carefully controlled
;;; circumstances they can be called when their class under test is not defined.
;;; The exact requirement is that the lowtag test must fail.
;;; So for example you can call GENERIC-FUNCTION-P on a HASH-TABLE,
;;; and CLASSP on a STRING, but you can't call CLASSP on anything that is either
;;; a FUNCALLABLE-INSTANCE or INSTANCE.
;;; With that caveat in mind, these are nifty things to have ASAP.
#-sb-xc-host
(macrolet ((define-class-predicates ()
             `(progn
                ,@(mapcar (lambda (x)
                           (destructuring-bind (class-name predicate) x
                             `(defun ,predicate (x) (typep x ',class-name))))
                         *!early-class-predicates*))))
  (define-class-predicates))
