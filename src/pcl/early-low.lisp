;;;; some code pulled out of CMU CL's low.lisp to solve build order problems,
;;;; and some other stuff that just plain needs to be done early

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



(/show0 "starting early-low.lisp")

;;; PCL instances

(sb!kernel::!defstruct-with-alternate-metaclass standard-instance
  ;; KLUDGE: arm64 needs to have CAS-HEADER-DATA-HIGH implemented
  :slot-names (slots #!-(and compact-instance-header x86-64) hash-code)
  :boa-constructor %make-standard-instance
  :superclass-name t
  :metaclass-name standard-classoid
  :metaclass-constructor make-standard-classoid
  :dd-type structure
  :runtime-type-checks-p nil)

;;; TODO: for x8-64 with #!+immobile-code, we would like 2 additional unboxed
;;; words to hold the trampline instructions to avoid consing a piece of code
;;; to jump to this function. It should be "as if" a simple-fun, in as much as
;;; there's an address you can jump to without loading a register.
(sb!kernel::!defstruct-with-alternate-metaclass standard-funcallable-instance
  ;; KLUDGE: Note that neither of these slots is ever accessed by its
  ;; accessor name as of sbcl-0.pre7.63. Presumably everything works
  ;; by puns based on absolute locations. Fun fun fun.. -- WHN 2001-10-30
  :slot-names (clos-slots #!-compact-instance-header hash-code)
  :boa-constructor %make-standard-funcallable-instance
  :superclass-name function
  :metaclass-name standard-classoid
  :metaclass-constructor make-standard-classoid
  :dd-type funcallable-structure
  ;; Only internal implementation code will access these, and these
  ;; accesses (slot readers in particular) could easily be a
  ;; bottleneck, so it seems reasonable to suppress runtime type
  ;; checks.
  ;;
  ;; (Except note KLUDGE above that these accessors aren't used at all
  ;; (!) as of sbcl-0.pre7.63, so for now it's academic.)
  :runtime-type-checks-p nil)

#!+(and compact-instance-header (not x86-64))
(defconstant std-instance-hash-slot-index 1)
#!-compact-instance-header
(progn
(defconstant std-instance-hash-slot-index 2)
;; The first data slot (either index 0 or 1) in the primitive funcallable
;; instance is the vector of CLOS slots. Following that is the hash.
(defconstant fsc-instance-hash-slot-index (1+ sb!vm:instance-data-start)))

(/show0 "finished with early-low.lisp")
