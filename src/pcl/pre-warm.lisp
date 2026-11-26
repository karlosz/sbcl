;;;; In support of PCL we compile some things into the cold image.
;;;; Not only does this simplify the PCL bootstrap ever so slightly,
;;;; it is nice to be able to test for types SB-PCL::%METHOD-FUNCTION
;;;; and CLASS (neither of which will have any instances too early).

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

(in-package "SB-PCL")

;;; Whether PCL's classes and generic functions work: always in the target,
;;; whose PCL genesis wrote.  On the host, once host PCL's pass 2 begins
;;; (RUN-PASS-2 in src/cold/host-pcl-dispatch.lisp): not while the
;;; cross-compiler expands PCL's macros as it compiles PCL, so their code
;;; does not count on classes.
#+sb-xc-host (defvar *host-pcl-ready* nil)
;;; ... and while it runs PCL's definitions (CALL-AS-HOST-PCL)
#+sb-xc-host (defvar *host-pcl-evaluating* nil)
(defmacro pcl-ready-p () #+sb-xc-host '*host-pcl-ready* #-sb-xc-host t)

;;; Toplevel forms of PCL proper's files that host PCL, which builds the
;;; classes and generic functions genesis writes (src/cold/host-pcl.lisp),
;;; runs later than where they are: !CAPTURING below.  Their forms are
;;; recorded as the host loads PCL proper's files (before it builds host
;;; PCL: !HOST-LOAD-RECORD, src/code/cross-early.lisp).
#+sb-xc-host (declaim (ftype function !host-load-record))
;;; The lambda list of the method a DEFMETHOD makes, which the
;;; cross-compiler notes (EXPAND-DEFMETHOD)
#+sb-xc-host (defvar *defmethod-lambda-list*)
(defmacro !cross-compiling-p () '(member :sb-xc sb-xc:*features*))
;;; (as the host loads PCL's files, rather than host PCL running them)
(defmacro !loading-pcl-proper-p ()
  #+sb-xc-host '(not (or (!cross-compiling-p) *host-pcl-evaluating*)) #-sb-xc-host nil)
;;; Forms that host PCL runs as it makes PCL's generic functions, once they
;;; exist and before they are initialized: LOAD-TIME-VALUE of one,
;;; braid.lisp's links of classoids to their classes, which initializing
;;; them needs, and dlisp3.lisp's dfun constructors, which are code
(defmacro !capturing (&whole form &body forms)
  #-sb-xc-host (declare (ignore form))
  #+sb-xc-host
  (when (!loading-pcl-proper-p)
    (return-from !capturing (!host-load-record :capturing form)))
  `(progn ,@forms))
;;; method function stuff.
;;;
;;; PCL historically included a so-called method-fast-function, which
;;; is essentially a method function but with (a) a precomputed
;;; continuation for CALL-NEXT-METHOD and (b) a permutation vector for
;;; slot access.  [ FIXME: see if we can understand these two
;;; optimizations before commit. ]  However, the presence of the
;;; fast-function meant that we violated AMOP and the effect of the
;;; :FUNCTION initarg, and furthermore got to potentially confusing
;;; situations where the function and the fast-function got out of
;;; sync, so that calling (method-function method) with the defined
;;; protocol would do different things from (call-method method) in
;;; method combination.
;;;
;;; So we define this internal method function structure, which we use
;;; when we create a method function ourselves.  This means that we
;;; can hang the various bits of information that we want off the
;;; method function itself, and also that if a user overrides method
;;; function creation there is no danger of having the system get
;;; confused.

#-sb-xc-host ; host doesn't need
(progn
(!defstruct-with-alternate-metaclass %method-function
  :slot-names (fast-function)
  :constructor %make-method-function
  :superclass-name function
  :metaclass-name static-classoid
  :metaclass-constructor make-static-classoid
  :dd-type funcallable-structure)
;; For #+executable-funinstances the hash code occupies 4 unused bytes in
;; the 2nd of the raw words containing the builtin trampoline instructions.
;; I don't know why it was ever important to save a whopping 1 word per function.
;; Maybe it forced alignment of machine instructions to a 16-byte boundary
;; when the builtin trampoline followed the CLOS slots?
(sb-kernel:!defstruct-with-alternate-metaclass standard-funcallable-instance
  :slot-names (clos-slots #-executable-funinstances hash-code)
  :constructor %make-standard-funcallable-instance
  :superclass-name function
  :metaclass-name static-classoid
  :metaclass-constructor make-static-classoid
  :dd-type funcallable-structure)
)

;;; The predicates of some of PCL's classes, below
(eval-when (:compile-toplevel :load-toplevel :execute)
(defparameter *!early-class-predicates*
  '((specializer specializerp)
    (standard-specializer standard-specializer-p)
    (exact-class-specializer exact-class-specializer-p)
    (class-eq-specializer class-eq-specializer-p)
    (eql-specializer eql-specializer-p)
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
    (global-reader-method global-reader-method-p)
    (global-writer-method global-writer-method-p)
    (global-boundp-method global-boundp-method-p)
    (global-makunbound-method global-makunbound-method-p)
    (generic-function generic-function-p)
    (standard-generic-function standard-generic-function-p)
    (method-combination method-combination-p)
    (long-method-combination long-method-combination-p)
    (short-method-combination short-method-combination-p))))

#+sb-xc-host
(progn
;;; Create #<SB-KERNEL::CONDITION-CLASSOID CONDITION>
;;; so that we can successfully parse the type specifier
;;; CONDITION-DESIGNATOR-HEAD which expands to
;;; (or format-control symbol condition sb-pcl::condition-class).
;;; Compiling any ERROR or WARN call eagerly looks up and re-parses
;;; the global ftype, and we don't want to see unknown types.
(let* ((name 'condition)
       (classoid (sb-kernel::make-condition-classoid :name name))
       (cell (sb-kernel::make-classoid-cell name classoid))
       (layout (make-layout (hash-layout-name name)
                             classoid
                             :depthoid 1
                             :inherits (vector (find-layout 't))
                             :length (+ sb-vm:instance-data-start 1)
                             :flags +condition-layout-flag+
                             :invalid nil)))
  (setf (classoid-layout classoid) layout
        (info :type :classoid-cell name) cell
        (info :type :kind name) :instance))
) ; end PROGN

;;; BIG FAT WARNING: These predicates can't in general be called prior to the
;;; definition of the class which they test. However in carefully controlled
;;; circumstances they can be called when their class under test is not defined.
;;; The exact requirement is that the lowtag test must fail.
;;; So for example you can call GENERIC-FUNCTION-P on a HASH-TABLE,
;;; and CLASSP on a STRING, but you can't call CLASSP on anything that is either
;;; a FUNCALLABLE-INSTANCE or INSTANCE.
;;; With that caveat in mind, these are nifty things to have ASAP.
;;; (On the host, of the host's PCL's objects: src/code/cross-pcl.lisp.)
(macrolet ((define-class-predicates ()
             `(progn
                ,@(mapcar (lambda (x)
                           (destructuring-bind (class-name predicate) x
                             `(defun ,predicate (x)
                                #+sb-xc-host (host-pcl-typep x ',class-name)
                                #-sb-xc-host (typep x ',class-name))))
                         *!early-class-predicates*))))
  (define-class-predicates))

(defun safe-code-p (&optional env)
  #+sb-xc-host (when (sb-c::host-environment-p env) (return-from safe-code-p nil))
  (sb-c::policy (or env (sb-c::make-null-lexenv)) (eql safety 3)))
