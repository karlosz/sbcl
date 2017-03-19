;;;; things which the main SBCL compiler needs to know about the
;;;; implementation of CLOS
;;;;
;;;; (Our CLOS is derived from PCL, which was implemented in terms of
;;;; portable high-level Common Lisp. But now that it no longer needs
;;;; to be portable, we can make some special hacks to support it
;;;; better.)

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

(in-package "SB!C")

;;;; very low-level representation of instances with meta-class
;;;; STANDARD-CLASS

(defun sb!pcl::safe-code-p (&optional env)
  #+sb-xc-host (declare (ignore env))
  #+sb-xc
  (policy (or env (make-null-lexenv)) (eql safety 3)))

(declaim (ftype function sb!pcl::parse-specialized-lambda-list))
(define-source-context defmethod (name &rest stuff)
  (let ((arg-pos (position-if #'listp stuff)))
    (if arg-pos
        `(defmethod ,name ,@(subseq stuff 0 arg-pos)
           ,(handler-case
                (nth-value 2 (sb!pcl::parse-specialized-lambda-list
                              (elt stuff arg-pos)))
              (error () "<illegal syntax>")))
        `(defmethod ,name "<illegal syntax>"))))

(defvar sb!pcl::*internal-pcl-generalized-fun-name-symbols* nil)

(defmacro define-internal-pcl-function-name-syntax (name (var) &body body)
  `(progn
     (define-function-name-syntax ,name (,var) ,@body)
     (pushnew ',name sb!pcl::*internal-pcl-generalized-fun-name-symbols*)))

(define-internal-pcl-function-name-syntax sb!pcl::slot-accessor (list)
  (when (= (length list) 4)
    (destructuring-bind (class slot rwb) (cdr list)
      (when (and (member rwb '(sb!pcl::reader sb!pcl::writer sb!pcl::boundp))
                 (symbolp slot)
                 (symbolp class))
        (values t slot)))))

#+sb-xc ;; TODO figure out why
(define-internal-pcl-function-name-syntax sb!pcl::fast-method (list)
  (valid-function-name-p (cadr list)))

(define-internal-pcl-function-name-syntax sb!pcl::slow-method (list)
  (valid-function-name-p (cadr list)))
