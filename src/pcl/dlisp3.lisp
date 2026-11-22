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


#-sb-xc-host ; not needed on host XXX? here temporarily!!!!
(progn

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
(defmethod no-applicable-method (generic-function &rest args)
  (error 'no-applicable-method-error
         :generic-function generic-function
         :args args))

(define-condition no-next-method-error (effective-method-error)
  ()
  (:default-initargs
   :problem "There is no next method"
   :references '((:ansi-cl :section (7 6 6 2)))))
(defmethod no-next-method ((generic-function standard-generic-function)
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
(defmethod no-primary-method (generic-function &rest args)
  (error 'no-primary-method-error
         :generic-function generic-function
         :args args))

;; FIXME shouldn't this specialize on STANDARD-METHOD-COMBINATION?
(defmethod invalid-qualifiers ((gf generic-function)
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

(defmethod compute-primary-methods ((gf generic-function)
                                    combin
                                    applicable-methods)
  applicable-methods)

)

;;; Rather than compiling the constructors here, just tickle the range
;;; of shapes defined above, leaving the generation of the
;;; constructors to precompile-dfun-constructors.
(let ((checking-or-caching-list
        '((t nil (class) nil)
          (t nil (class class) nil)
          (t nil (class class class) nil)
          (t nil (class class t) nil)
          (t nil (class class t t) nil)
          (t nil (class class t t t) nil)
          (t nil (class t) nil)
          (t nil (class t t) nil)
          (t nil (class t t t) nil)
          (t nil (class t t t t) nil)
          (t nil (class t t t t t) nil)
          (t nil (class t t t t t t) nil)
          (t nil (t class) nil)
          (t nil (t class t) nil)
          (t nil (t t class) nil)
          (t nil (class) t)
          (t nil (class class) t)
          (t nil (class t) t)
          (t nil (class t t) t)
          (t nil (class t t t) t)
          (t nil (t class) t)
          (t t (class) nil)
          (t t (class class) nil)
          (t t (class class class) nil)
          (nil nil (class) nil)
          (nil nil (class class) nil)
          (nil nil (class class t) nil)
          (nil nil (class class t t) nil)
          (nil nil (class t) nil)
          (nil nil (t class t) nil)
          (nil nil (class) t)
          (nil nil (class class) t))))
  (dolist (key checking-or-caching-list)
    (destructuring-bind (cached-emf-p return-value-p metatypes applyp) key
      (multiple-value-bind (args generator)
          (if cached-emf-p
              (if return-value-p
                  (values (list metatypes) 'emit-constant-value)
                  (values (list metatypes applyp) 'emit-caching))
              (if return-value-p
                  (values (list metatypes) 'emit-in-checking-p)
                  (values (list metatypes applyp) 'emit-checking)))
        (apply #'get-dfun-constructor generator args)))))
