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


;;; FIXME -- we will probably need to maintain a list of these and
;;; rename them to the target platform's preferred format at cold init
;;; time.
#+sb-xc-host
(defun make-slot-symbol (slot-name type)
  #!+sb-doc
  "Generates a symbol to use as the reader/writer/boundp function for
a particular slot."
  (declare (type symbol slot-name))
  (unless (symbol-package slot-name)
    (error "On the host, slot symbols must have a package and ~S lacks one" slot-name))
  (intern (format nil "~A::~A slot ~a"
                        (package-name (symbol-package slot-name))
                        (symbol-name slot-name)
                        type)
          *pcl-package*)) ;*slot-accessor-name-package*)))

;; This choice of naming structure is perhaps unfortunate, because were the
;; names 2-lists, the globaldb hack to support this would instead be
;; a natural use of the (SETF <x>) style naming that globaldb favors.
;; But this naming is documented, and changing it would be incompatible.
;; The 4-part name can be thought of as a 2-part name because
;; half of it is composed of constants:
;; (SB-PCL::SLOT-ACCESSOR :GLOBAL <foo> SB-PCL::{READER|WRITER|BOUNDP})
;; -> ({READER|WRITER|BOUNDP} <foo>)
;;
(defun slot-reader-name (slot-name)
  #+sb-xc
  (list 'slot-accessor :global slot-name 'reader)
  #+sb-xc-host
  (make-slot-symbol slot-name 'reader))

(defun slot-writer-name (slot-name)
  #+sb-xc
  (list 'slot-accessor :global slot-name 'writer)
  #+sb-xc-host
  (make-slot-symbol slot-name 'write))

(defun slot-boundp-name (slot-name)
  #+sb-xc
  (list 'slot-accessor :global slot-name 'boundp)
  #+sb-xc-host
  (make-slot-symbol slot-name 'boundp))

;;; This is the value that we stick into a slot to tell us that it is
;;; unbound. It may seem gross, but for performance reasons, we make
;;; this an interned symbol. That means that the fast check to see
;;; whether a slot is unbound is to say (EQ <val> '..SLOT-UNBOUND..).
;;; That is considerably faster than looking at the value of a special
;;; variable.
;;;
;;; It seems only reasonable to also export this for users, since
;;; otherwise dealing with STANDARD-INSTANCE-ACCESS becomes harder
;;; -- and slower -- than it needs to be.
(defconstant +slot-unbound+ '..slot-unbound..
  "SBCL specific extensions to MOP: if this value is read from an
instance using STANDARD-INSTANCE-ACCESS, the slot is unbound.
Similarly, an :INSTANCE allocated slot can be made unbound by
assigning this to it using (SETF STANDARD-INSTANCE-ACCESS).

Value of +SLOT-UNBOUND+ is unspecified, and should not be relied to be
of any particular type, but it is guaranteed to be suitable for EQ
comparison.")
