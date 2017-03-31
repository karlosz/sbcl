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

;;;; early generic function support
(defvar *!early-generic-functions* ())

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

(sb-xc:defmacro defgeneric (fun-name lambda-list &body options)
  (declare (type list lambda-list))
  (unless (legal-fun-name-p fun-name)
    (error 'simple-program-error
           :format-control "illegal generic function name ~S"
           :format-arguments (list fun-name)))
  (check-gf-lambda-list lambda-list)
  (let ((initargs ())
        (methods ()))
    (flet ((duplicate-option (name)
             (error 'simple-program-error
                    :format-control "The option ~S appears more than once."
                    :format-arguments (list name)))
           (expand-method-definition (qab) ; QAB = qualifiers, arglist, body
             (let* ((arglist-pos (position-if #'listp qab))
                    (arglist (elt qab arglist-pos))
                    (qualifiers (subseq qab 0 arglist-pos))
                    (body (nthcdr (1+ arglist-pos) qab)))
               `(push (defmethod ,fun-name ,@qualifiers ,arglist ,@body)
                      (generic-function-initial-methods (fdefinition ',fun-name))))))
      (macrolet ((initarg (key) `(getf initargs ,key)))
        (dolist (option options)
          (let ((car-option (car option)))
            (case car-option
              (declare
               (dolist (spec (cdr option))
                 (unless (consp spec)
                   (error 'simple-program-error
                          :format-control "~@<Invalid declaration specifier in ~
                                           DEFGENERIC: ~S~:@>"
                          :format-arguments (list spec)))
                 (when (member (first spec)
                               ;; FIXME: this list is slightly weird.
                               ;; ANSI (on the DEFGENERIC page) in one
                               ;; place allows only OPTIMIZE; in
                               ;; another place gives this list of
                               ;; disallowed declaration specifiers.
                               ;; This seems to be the only place where
                               ;; the FUNCTION declaration is
                               ;; mentioned; TYPE seems to be missing.
                               ;; Very strange.  -- CSR, 2002-10-21
                               '(declaration ftype function
                                 inline notinline special))
                   (error 'simple-program-error
                          :format-control "The declaration specifier ~S ~
                                         is not allowed inside DEFGENERIC."
                          :format-arguments (list spec)))
                 (if (or (eq 'optimize (first spec))
                         (info :declaration :recognized (first spec)))
                     (push spec (initarg :declarations))
                     (warn "Ignoring unrecognized declaration in DEFGENERIC: ~S"
                           spec))))
              (:method-combination
               (when (initarg car-option)
                 (duplicate-option car-option))
               (unless (symbolp (cadr option))
                 (error 'simple-program-error
                        :format-control "METHOD-COMBINATION name not a ~
                                         symbol: ~S"
                        :format-arguments (list (cadr option))))
               (setf (initarg car-option)
                     `',(cdr option)))
              (:argument-precedence-order
               (let* ((required (nth-value 1 (parse-lambda-list lambda-list)))
                      (supplied (cdr option)))
                 (unless (= (length required) (length supplied))
                   (error 'simple-program-error
                          :format-control "argument count discrepancy in ~
                                           :ARGUMENT-PRECEDENCE-ORDER clause."
                          :format-arguments nil))
                 (when (set-difference required supplied)
                   (error 'simple-program-error
                          :format-control "unequal sets for ~
                                           :ARGUMENT-PRECEDENCE-ORDER clause: ~
                                           ~S and ~S"
                          :format-arguments (list required supplied)))
                 (setf (initarg car-option)
                       `',(cdr option))))
              ((:documentation :generic-function-class :method-class)
               (unless (proper-list-of-length-p option 2)
                 (error "bad list length for ~S" option))
               (if (initarg car-option)
                   (duplicate-option car-option)
                   (setf (initarg car-option) `',(cadr option))))
              (:method
               (push (cdr option) methods))
              (t
               ;; ANSI requires that unsupported things must get a
               ;; PROGRAM-ERROR.
               (error 'simple-program-error
                      :format-control "unsupported option ~S"
                      :format-arguments (list option))))))

        (when (initarg :declarations)
          (setf (initarg :declarations)
                `',(initarg :declarations))))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (compile-or-load-defgeneric ',fun-name))
         (load-defgeneric ',fun-name ',lambda-list
                          (sb!c:source-location) ,@initargs)
         ,@(mapcar #'expand-method-definition methods)
         (fdefinition ',fun-name)))))

(defun compile-or-load-defgeneric (fun-name)
  (proclaim-as-fun-name fun-name)
  (when (typep fun-name '(cons (eql setf)))
    (sb!c::warn-if-setf-macro fun-name))
  (note-name-defined fun-name :function)
  (unless (eq (info :function :where-from fun-name) :declared)
    ;; Hmm. This is similar to BECOME-DEFINED-FUN-NAME
    ;; except that it doesn't clear an :ASSUMED-TYPE. Should it?
    (setf (info :function :where-from fun-name) :defined)
    (setf (info :function :type fun-name)
          (specifier-type 'function))))

(defun load-defgeneric (fun-name lambda-list source-location &rest initargs)
  (when (fboundp fun-name)
    (warn 'sb!kernel:redefinition-with-defgeneric
          :name fun-name
          :new-location source-location)
    (let ((fun (fdefinition fun-name)))
      (when (generic-function-p fun)
        (loop for method in (generic-function-initial-methods fun)
              do (remove-method fun method))
        (setf (generic-function-initial-methods fun) '()))))
  (apply #'sb-xc:ensure-generic-function
         fun-name
         :lambda-list lambda-list
         :definition-source source-location
         initargs))

(define-condition generic-function-lambda-list-error
    (reference-condition simple-program-error)
  ()
  (:default-initargs :references (list '(:ansi-cl :section (3 4 2)))))

(defun check-gf-lambda-list (lambda-list)
  (flet ((verify-each-atom-or-singleton (kind args)
           ;; PARSE-LAMBDA-LIST validates the skeleton,
           ;; so just check for incorrect use of defaults.
           ;; This works for both &OPTIONAL and &KEY.
           (dolist (arg args)
             (or (not (listp arg))
                 (null (cdr arg))
                 (error 'generic-function-lambda-list-error
                    :format-control
                    "~@<invalid ~A argument specifier ~S ~_in the ~
generic function lambda list ~S~:>"
                    :format-arguments (list kind arg lambda-list))))))
    (multiple-value-bind (llks required optional rest keys)
        (parse-lambda-list
         lambda-list
         :accept (lambda-list-keyword-mask
                    '(&optional &rest &key &allow-other-keys))
         :condition-class 'generic-function-lambda-list-error
         :context "a generic function lambda list")
      (declare (ignore llks required rest))
      ;; no defaults or supplied-p vars allowed for &OPTIONAL or &KEY
      (verify-each-atom-or-singleton '&optional optional)
      (verify-each-atom-or-singleton '&key keys))))

(defun check-method-lambda (method-lambda context)
  (unless (typep method-lambda '(cons (eql lambda)))
    (error "~@<The METHOD-LAMBDA argument to ~
            ~/sb!impl:print-symbol-with-prefix/, ~S, is not a lambda ~
            form.~@:>"
           context method-lambda))
  method-lambda)

;;;; ensure-generic-function and friends

;; CLHS doesn't specify &allow-other-keys here but I guess the supposition
;; is that they'll be checked by ENSURE-GENERIC-FUNCTION-USING-CLASS.
;; Except we don't do that either, so I think the blame, if any, lies there
;; for not catching errant keywords.

(defun sb-xc:ensure-generic-function (fun-name &rest all-keys)
  (let ((existing (and (fboundp fun-name)
                       (gdefinition fun-name))))
    (cond ((and existing
                (eq **boot-state** 'complete)
                (null (generic-function-p existing)))
           (generic-clobbers-function fun-name)
           (fmakunbound fun-name)
           (apply #'sb-xc:ensure-generic-function fun-name all-keys))
          (t
           (apply #'ensure-generic-function-using-class
                  existing fun-name all-keys)))))

(defun generic-clobbers-function (fun-name)
  (cerror "Replace the function binding"
          'simple-program-error
          :format-control "~@<~/sb!impl:print-symbol-with-prefix/ ~
                           already names an ordinary function or a ~
                           macro.~@:>"
          :format-arguments (list fun-name)))

(defvar *sgf-wrapper*
  (!boot-make-wrapper (!early-class-size 'standard-generic-function)
                      'standard-generic-function))

(defvar *sgf-slots-init*
  (mapcar (lambda (canonical-slot)
            (if (memq (getf canonical-slot :name) '(arg-info source))
                +slot-unbound+
                (let ((initfunction (getf canonical-slot :initfunction)))
                  (if initfunction
                      (funcall initfunction)
                      +slot-unbound+))))
          (!early-collect-inheritance 'standard-generic-function)))

(defconstant +sgf-method-class-index+
  (!bootstrap-slot-index 'standard-generic-function 'method-class))

(defun early-gf-p (x)
  (and (fsc-instance-p x)
       (eq (clos-slots-ref (get-slots x) +sgf-method-class-index+)
           +slot-unbound+)))

(defconstant +sgf-methods-index+
  (!bootstrap-slot-index 'standard-generic-function 'methods))

(defmacro early-gf-methods (gf)
  `(clos-slots-ref (get-slots ,gf) +sgf-methods-index+))

(defun safe-generic-function-methods (generic-function)
  (if (eq (class-of generic-function) *the-class-standard-generic-function*)
      (clos-slots-ref (get-slots generic-function) +sgf-methods-index+)
      (generic-function-methods generic-function)))

(defconstant +sgf-arg-info-index+
  (!bootstrap-slot-index 'standard-generic-function 'arg-info))

(defmacro early-gf-arg-info (gf)
  `(clos-slots-ref (get-slots ,gf) +sgf-arg-info-index+))

(defconstant +sgf-dfun-state-index+
  (!bootstrap-slot-index 'standard-generic-function 'dfun-state))

;; a few symbols and their associated code that would otherwise be retained:
;;  *!EARLY-{GENERIC-}FUNCTIONS*, *!GENERIC-FUNCTION-FIXUPS*
(defun early-gf-primary-slow-method-fn (fn)
  (lambda (args next-methods)
    (declare (ignore next-methods))
    (apply fn args)))

;;;; Arg info
(defstruct (arg-info
            (:conc-name nil)
            (:constructor make-arg-info ())
            (:copier nil))
  (arg-info-lambda-list :no-lambda-list)
  arg-info-precedence
  arg-info-metatypes
  arg-info-number-optional
  arg-info-key/rest-p
  arg-info-keys   ;nil        no &KEY or &REST allowed
                  ;(k1 k2 ..) Each method must accept these &KEY arguments.
                  ;T          must have &KEY or &REST

  gf-info-simple-accessor-type ; nil, reader, writer, boundp
  (gf-precompute-dfun-and-emf-p nil) ; set by set-arg-info

  gf-info-static-c-a-m-emf
  (gf-info-c-a-m-emf-std-p t)
  gf-info-fast-mf-p)

#!-sb-fluid (declaim (sb!ext:freeze-type arg-info))

(defun arg-info-valid-p (arg-info)
  (not (null (arg-info-number-optional arg-info))))

(defun arg-info-applyp (arg-info)
  (or (plusp (arg-info-number-optional arg-info))
      (arg-info-key/rest-p arg-info)))

(defun arg-info-number-required (arg-info)
  (length (arg-info-metatypes arg-info)))

(defun arg-info-nkeys (arg-info)
  (count-if (lambda (x) (neq x t)) (arg-info-metatypes arg-info)))

(defun create-gf-lambda-list (lambda-list)
  ;;; Create a gf lambda list from a method lambda list
  (loop for x in lambda-list
        collect (if (consp x) (list (car x)) x)
        if (eq x '&key) do (loop-finish)))

(defun ll-keyp-or-restp (bits)
  (logtest (lambda-list-keyword-mask '(&key &rest)) bits))

(defun remove-methods (gf)
  (loop for method in (generic-function-methods gf)
        do (remove-method gf method)))

(defun set-arg-info (gf &key new-method (lambda-list nil lambda-list-p)
                        argument-precedence-order)
  (let* ((arg-info (if (eq **boot-state** 'complete)
                       (gf-arg-info gf)
                       (early-gf-arg-info gf)))
         (methods (if (eq **boot-state** 'complete)
                      (generic-function-methods gf)
                      (early-gf-methods gf)))
         (was-valid-p (integerp (arg-info-number-optional arg-info)))
         (first-p (and new-method (null (cdr methods)))))
    (when (and (not lambda-list-p) methods)
      (setq lambda-list (gf-lambda-list gf)))
    (when (or lambda-list-p
              (and first-p
                   (eq (arg-info-lambda-list arg-info) :no-lambda-list)))
      (multiple-value-bind (llks nreq nopt keywords)
          (analyze-lambda-list lambda-list)
        (when (and methods (not first-p))
          (let ((gf-nreq (arg-info-number-required arg-info))
                (gf-nopt (arg-info-number-optional arg-info))
                (gf-key/rest-p (arg-info-key/rest-p arg-info)))
            (unless (and (= nreq gf-nreq)
                         (= nopt gf-nopt)
                         (eq (ll-keyp-or-restp llks) gf-key/rest-p))
              (restart-case
                  (error "New lambda-list ~S is incompatible with ~
                          existing methods of ~S.~%~
                          Old lambda-list ~s"
                         lambda-list gf (arg-info-lambda-list arg-info))
                (continue ()
                  :report "Remove all methods."
                  (remove-methods gf))))))
        (setf (arg-info-lambda-list arg-info)
              (if lambda-list-p
                  lambda-list
                   (create-gf-lambda-list lambda-list)))
        (when (or lambda-list-p argument-precedence-order
                  (null (arg-info-precedence arg-info)))
          (setf (arg-info-precedence arg-info)
                (compute-precedence lambda-list nreq argument-precedence-order)))
        (setf (arg-info-metatypes arg-info) (make-list nreq))
        (setf (arg-info-number-optional arg-info) nopt)
        (setf (arg-info-key/rest-p arg-info) (ll-keyp-or-restp llks))
        (setf (arg-info-keys arg-info)
              (if lambda-list-p
                  (if (ll-kwds-allowp llks) t keywords)
                  (arg-info-key/rest-p arg-info)))))
    (when new-method
      (check-method-arg-info gf arg-info new-method))
    (set-arg-info1 gf arg-info new-method methods was-valid-p first-p)
    arg-info))

(defun check-method-arg-info (gf arg-info method)
  (multiple-value-bind (llks nreq nopt keywords)
      (analyze-lambda-list (if (consp method)
                               (early-method-lambda-list method)
                               (method-lambda-list method)))
    (flet ((lose (string &rest args)
             (error 'simple-program-error
                    :format-control "~@<attempt to add the method~2I~_~S~I~_~
                                     to the generic function~2I~_~S;~I~_~
                                     but ~?~:>"
                    :format-arguments (list method gf string args)))
           (comparison-description (x y)
             (if (> x y) "more" "fewer")))
      (let ((gf-nreq (arg-info-number-required arg-info))
            (gf-nopt (arg-info-number-optional arg-info))
            (gf-key/rest-p (arg-info-key/rest-p arg-info))
            (gf-keywords (arg-info-keys arg-info)))
        (unless (= nreq gf-nreq)
          (lose
           "the method has ~A required arguments than the generic function."
           (comparison-description nreq gf-nreq)))
        (unless (= nopt gf-nopt)
          (lose
           "the method has ~A optional arguments than the generic function."
           (comparison-description nopt gf-nopt)))
        (unless (eq (ll-keyp-or-restp llks) gf-key/rest-p)
          (lose
           "the method and generic function differ in whether they accept~_~
            &REST or &KEY arguments."))
        (when (consp gf-keywords)
          (unless (or (and (ll-kwds-restp llks) (not (ll-kwds-keyp llks)))
                      (ll-kwds-allowp llks)
                      (every (lambda (k) (memq k keywords)) gf-keywords))
            (lose "the method does not accept each of the &KEY arguments~2I~_~
                   ~S."
                  gf-keywords)))))))


;;; This is the early definition of ENSURE-GENERIC-FUNCTION-USING-CLASS.
;;;
;;; The STATIC-SLOTS field of the funcallable instances used as early
;;; generic functions is used to store the early methods and early
;;; discriminator code for the early generic function. The static
;;; slots field of the fins contains a list whose:
;;;    CAR    -   a list of the early methods on this early gf
;;;    CADR   -   the early discriminator code for this method
(defun ensure-generic-function-using-class (existing spec &rest keys
                                            &key (lambda-list nil
                                                              lambda-list-p)
                                            argument-precedence-order
                                            definition-source
                                            documentation
                                            &allow-other-keys)
  (declare (ignore keys))
  (cond ((and existing (early-gf-p existing))
         (when lambda-list-p
           (set-arg-info existing :lambda-list lambda-list))
         existing)
        ((assoc spec *!generic-function-fixups* :test #'equal)
         (if existing
             (make-early-gf spec lambda-list lambda-list-p existing
                            argument-precedence-order definition-source
                            documentation)
             (bug "The function ~S is not already defined." spec)))
        (existing
         (bug "~S should be on the list ~S."
              spec '*!generic-function-fixups*))
        (t
         (pushnew spec *!early-generic-functions* :test #'equal)
         (make-early-gf spec lambda-list lambda-list-p nil
                        argument-precedence-order definition-source
                        documentation))))

(defun make-early-gf (spec &optional lambda-list lambda-list-p
                      function argument-precedence-order source-location
                      documentation)
  (let ((fin (allocate-standard-funcallable-instance *sgf-wrapper*)))
    (replace (fsc-instance-slots fin) *sgf-slots-init*)
    (when function
      (set-funcallable-instance-function fin function))
    (setf (gdefinition spec) fin)
    (!bootstrap-set-slot 'standard-generic-function fin 'name spec)
    (!bootstrap-set-slot 'standard-generic-function fin
                         'source source-location)
    (!bootstrap-set-slot 'standard-generic-function fin
                         '%documentation documentation)
    (let ((arg-info (make-arg-info)))
      (setf (early-gf-arg-info fin) arg-info)
      (when lambda-list-p
        (setf (info :function :type spec)
              (specifier-type
               (ftype-declaration-from-lambda-list lambda-list spec))
              (info :function :where-from spec) :defined-method)
        (if argument-precedence-order
            (set-arg-info fin
                          :lambda-list lambda-list
                          :argument-precedence-order argument-precedence-order)
            (set-arg-info fin :lambda-list lambda-list))))
    fin))

(defun safe-gf-dfun-state (generic-function)
  (if (eq (class-of generic-function) *the-class-standard-generic-function*)
      (clos-slots-ref (fsc-instance-slots generic-function) +sgf-dfun-state-index+)
      (gf-dfun-state generic-function)))
(defun (setf safe-gf-dfun-state) (new-value generic-function)
  (if (eq (class-of generic-function) *the-class-standard-generic-function*)
      (setf (clos-slots-ref (fsc-instance-slots generic-function)
                            +sgf-dfun-state-index+)
            new-value)
      (setf (gf-dfun-state generic-function) new-value)))

(defun set-dfun (gf &optional dfun cache info)
  (let ((new-state (if (and dfun (or cache info))
                       (list* dfun cache info)
                       dfun)))
    (cond
      ((eq **boot-state** 'complete)
       ;; Check that we are under the lock.
       #!+sb-thread
       (aver (eq sb!thread:*current-thread* (sb!thread:mutex-owner (gf-lock gf))))
       (setf (safe-gf-dfun-state gf) new-state))
      (t
       (setf (clos-slots-ref (get-slots gf) +sgf-dfun-state-index+)
             new-state))))
  dfun)

(defun gf-dfun-cache (gf)
  (let ((state (if (eq **boot-state** 'complete)
                   (safe-gf-dfun-state gf)
                   (clos-slots-ref (get-slots gf) +sgf-dfun-state-index+))))
    (typecase state
      (function nil)
      (cons (cadr state)))))

(defun gf-dfun-info (gf)
  (let ((state (if (eq **boot-state** 'complete)
                   (safe-gf-dfun-state gf)
                   (clos-slots-ref (get-slots gf) +sgf-dfun-state-index+))))
    (typecase state
      (function nil)
      (cons (cddr state)))))

(defconstant +sgf-name-index+
  (!bootstrap-slot-index 'standard-generic-function 'name))

(defun !early-gf-name (gf)
  (clos-slots-ref (get-slots gf) +sgf-name-index+))

(defun gf-lambda-list (gf)
  (let ((arg-info (if (eq **boot-state** 'complete)
                      (gf-arg-info gf)
                      (early-gf-arg-info gf))))
    (if (eq :no-lambda-list (arg-info-lambda-list arg-info))
        (let ((methods (if (eq **boot-state** 'complete)
                           (generic-function-methods gf)
                           (early-gf-methods gf))))
          (if (null methods)
              (progn
                (warn "no way to determine the lambda list for ~S" gf)
                nil)
              (let* ((method (car (last methods)))
                     (ll (if (consp method)
                             (early-method-lambda-list method)
                             (method-lambda-list method))))
                (create-gf-lambda-list ll))))
        (arg-info-lambda-list arg-info))))

(defun note-gf-signature (fun-name lambda-list-p lambda-list)
  (unless lambda-list-p
    ;; Use the existing lambda-list, if any. It is reasonable to do eg.
    ;;
    ;;   (if (fboundp name)
    ;;       (ensure-generic-function name)
    ;;       (ensure-generic-function name :lambda-list '(foo)))
    ;;
    ;; in which case we end up here with no lambda-list in the first leg.
    (setf (values lambda-list lambda-list-p)
          (handler-case
              (values (generic-function-lambda-list (fdefinition fun-name))
                      t)
            ((or warning error) ()
              (values nil nil)))))
  (let ((gf-type
         (specifier-type
          (if lambda-list-p
              (ftype-declaration-from-lambda-list lambda-list fun-name)
              'function)))
        (old-type nil))
    ;; FIXME: Ideally we would like to not clobber it, but because generic
    ;; functions assert their FTYPEs callers believing the FTYPE are left with
    ;; unsafe assumptions. Hence the clobbering. Be quiet when the new type
    ;; is a subtype of the old one, though -- even though the type is not
    ;; trusted anymore, the warning is still not quite as interesting.
    (when (and (eq :declared (info :function :where-from fun-name))
               (not (csubtypep gf-type (setf old-type (proclaimed-ftype fun-name)))))
      (style-warn "~@<Generic function ~S clobbers an earlier ~S proclamation ~S ~
                   for the same name with ~S.~:@>"
                  fun-name 'ftype
                  (type-specifier old-type)
                  (type-specifier gf-type)))
    (setf (info :function :type fun-name) gf-type
          (info :function :where-from fun-name) :defined-method)
    fun-name))

(labels ((resolve-class (context class-or-name environment)
           (cond ((symbolp class-or-name)
                  (sb-xc:find-class class-or-name t environment))
                 ((classp class-or-name)
                  class-or-name)
                 (t
                  (error "~@<The ~A (~S) was neither a class nor a ~
                          symbol that names a class.~@:>"
                         context class-or-name))))
         (resolve-and-finalize-class (class-or-name environment)
           (let ((class (resolve-class ":GENERIC-FUNCTION-CLASS argument"
                                       class-or-name environment)))
             (if (class-has-a-forward-referenced-superclass-p class)
                 ;; FIXME: reference MOP documentation -- this is an
                 ;; additional requirement on our users
                 (error "~@<The generic function class ~A is not ~
                         finalizeable~@:>"
                        class)
                 (ensure-class-finalized class))))
         (normalize-options (&rest options &key
                                   environment
                                   (lambda-list nil lambda-list-p)
                                   (generic-function-class 'standard-generic-function)
                                   &allow-other-keys)
           (let ((class (resolve-and-finalize-class
                         generic-function-class environment)))
             (collect ((initargs))
               (doplist (key value) options
                 (case key
                   ((:environment :generic-function-class))
                   (:method-combination
                    (initargs
                     key
                     (etypecase value
                       (cons
                        (destructuring-bind (type . options) value
                          (find-method-combination
                           (class-prototype class) type options)))
                       (method-combination
                        value))))
                   (:method-class
                    (initargs key (resolve-class ":METHOD-CLASS argument"
                                                 value environment)))
                   (t
                    (initargs key value))))
               (values class lambda-list lambda-list-p (initargs))))))

  (defun real-ensure-gf-using-class--generic-function
      (existing fun-name &rest options &key &allow-other-keys)
    (multiple-value-bind
          (generic-function-class lambda-list lambda-list-p initargs)
        (apply #'normalize-options options)
      (unless (eq (class-of existing) generic-function-class)
        (change-class existing generic-function-class))
      (prog1
          (apply #'reinitialize-instance existing initargs)
        (note-gf-signature fun-name lambda-list-p lambda-list))))

  (defun real-ensure-gf-using-class--null
      (existing fun-name &rest options &key &allow-other-keys)
    (declare (ignore existing))
    (multiple-value-bind
          (generic-function-class lambda-list lambda-list-p initargs)
        (apply #'normalize-options options)
      (prog1
          (setf (gdefinition fun-name)
                (apply #'make-instance generic-function-class
                       :name fun-name initargs))
        (note-gf-signature fun-name lambda-list-p lambda-list)))))

(defun safe-gf-arg-info (generic-function)
  (if (eq (class-of generic-function) *the-class-standard-generic-function*)
      (clos-slots-ref (fsc-instance-slots generic-function)
                      +sgf-arg-info-index+)
      (gf-arg-info generic-function)))

;;; FIXME: this function took on a slightly greater role than it
;;; previously had around 2005-11-02, when CSR fixed the bug whereby
;;; having more than one subclass of standard-generic-function caused
;;; the whole system to die horribly through a metacircle in
;;; GF-ARG-INFO.  The fix is to be slightly more disciplined about
;;; calling accessor methods -- we call GET-GENERIC-FUN-INFO when
;;; computing discriminating functions, so we need to be careful about
;;; having a base case for the recursion, and we provide that with the
;;; STANDARD-GENERIC-FUNCTION case below.  However, we are not (yet)
;;; as disciplined as CLISP's CLOS/MOP, and it would be nice to get to
;;; that stage, where all potentially dangerous cases are enumerated
;;; and stopped.  -- CSR, 2005-11-02.
(defun get-generic-fun-info (gf)
  ;; values   nreq applyp metatypes nkeys arg-info
  (multiple-value-bind (applyp metatypes arg-info)
      (let* ((arg-info (if (early-gf-p gf)
                           (early-gf-arg-info gf)
                           (safe-gf-arg-info gf)))
             (metatypes (arg-info-metatypes arg-info)))
        (values (arg-info-applyp arg-info)
                metatypes
                arg-info))
    (let ((nreq 0)
          (nkeys 0))
      (declare (fixnum nreq nkeys))
      (dolist (x metatypes)
        (incf nreq)
        (unless (eq x t)
          (incf nkeys)))
      (values nreq applyp metatypes
              nkeys
              arg-info))))

(defun generic-function-nreq (gf)
  (let* ((arg-info (if (early-gf-p gf)
                       (early-gf-arg-info gf)
                       (safe-gf-arg-info gf)))
         (metatypes (arg-info-metatypes arg-info)))
    (declare (list metatypes))
    (length metatypes)))
