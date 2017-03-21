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
#+nil
(eval-when (:compile-toplevel :load-toplevel :execute)
  (fmakunbound 'defmethod))
;;; As per CLHS -
;;; "defmethod is not required to perform any compile-time side effects."
;;; and we don't do much other than to make the function be defined,
;;; which means that checking of callers' arglists can only occur after called
;;; methods are actually loaded.
(sb-xc:defmacro defmethod (name &rest args)
  (multiple-value-bind (qualifiers lambda-list body)
      (parse-defmethod args)
    `(progn
       (eval-when (:compile-toplevel :execute)
         ;; :compile-toplevel is needed for subsequent forms
         ;; :execute is needed for references to itself inside the body
         (compile-or-load-defgeneric ',name))
       ;; KLUDGE: this double expansion is quite a monumental
       ;; workaround: it comes about because of a fantastic interaction
       ;; between the processing rules of CLHS 3.2.3.1 and the
       ;; bizarreness of MAKE-METHOD-LAMBDA.
       ;;
       ;; MAKE-METHOD-LAMBDA can be called by the user, and if the
       ;; lambda itself doesn't refer to outside bindings the return
       ;; value must be compileable in the null lexical environment.
       ;; However, the function must also refer somehow to the
       ;; associated method object, so that it can call NO-NEXT-METHOD
       ;; with the appropriate arguments if there is no next method --
       ;; but when the function is generated, the method object doesn't
       ;; exist yet.
       ;;
       ;; In order to resolve this issue, we insert a literal cons cell
       ;; into the body of the method lambda, return the same cons cell
       ;; as part of the second (initargs) return value of
       ;; MAKE-METHOD-LAMBDA, and a method on INITIALIZE-INSTANCE fills
       ;; in the cell when the method is created.  However, this
       ;; strategy depends on having a fresh cons cell for every method
       ;; lambda, which (without the workaround below) is skewered by
       ;; the processing in CLHS 3.2.3.1, which permits implementations
       ;; to macroexpand the bodies of EVAL-WHEN forms with both
       ;; :COMPILE-TOPLEVEL and :LOAD-TOPLEVEL only once.  The
       ;; expansion below forces the double expansion in those cases,
       ;; while expanding only once in the common case.
       (eval-when (:load-toplevel)
         (%defmethod-expander ,name ,qualifiers ,lambda-list ,body))
       (eval-when (:execute)
         (%defmethod-expander ,name ,qualifiers ,lambda-list ,body)))))

;;; PARSE-DEFMETHOD is used by DEFMETHOD to parse the &REST argument
;;; into the 'real' arguments. This is where the syntax of DEFMETHOD
;;; is really implemented.
(defun parse-defmethod (cdr-of-form)
  (declare (list cdr-of-form))
  (let ((qualifiers ())
        (spec-ll ()))
    (loop (if (and (car cdr-of-form) (atom (car cdr-of-form)))
              (push (pop cdr-of-form) qualifiers)
              (return (setq qualifiers (nreverse qualifiers)))))
    (setq spec-ll (pop cdr-of-form))
    (values qualifiers spec-ll cdr-of-form)))

(defun parse-specializers (generic-function specializers)
  (declare (list specializers))
  (flet ((parse (spec)
           (parse-specializer-using-class generic-function spec)))
    (mapcar #'parse specializers)))

(defun unparse-specializers (generic-function specializers)
  (declare (list specializers))
  (flet ((unparse (spec)
           (unparse-specializer-using-class generic-function spec)))
    (mapcar #'unparse specializers)))

(macrolet ((def (n name)
             `(defun ,name (lambda-list)
                (nth-value ,n (parse-specialized-lambda-list lambda-list)))))
  ;; We don't need these, but according to the unit tests,
  ;; they're mandated by AMOP.
  (def 1 extract-lambda-list)
  (def 2 extract-specializer-names))

(define-condition specialized-lambda-list-error
    (reference-condition simple-program-error)
  ()
  (:default-initargs :references (list '(:ansi-cl :section (3 4 3)))))

;; Return 3 values:
;; - the bound variables, without defaults, supplied-p vars, or &AUX vars.
;; - the lambda list without specializers.
;; - just the specializers
(defun parse-specialized-lambda-list (arglist)
  (multiple-value-bind (llks specialized optional rest key aux)
      (parse-lambda-list
       arglist
       :context 'defmethod
       :accept (lambda-list-keyword-mask
                '(&optional &rest &key &allow-other-keys &aux))
       :silent t ; never signal &OPTIONAL + &KEY style-warning
       :condition-class 'specialized-lambda-list-error)
    (let ((required (mapcar (lambda (x) (if (listp x) (car x) x)) specialized)))
      (values (append required
                      (mapcar #'parse-optional-arg-spec optional)
                      rest
                      ;; Preserve keyword-names when given as (:KEYWORD var)
                      (mapcar (lambda (x) (if (typep x '(cons cons))
                                              (car x)
                                              (parse-key-arg-spec x))) key))
              (make-lambda-list llks nil required optional rest key aux)
              (mapcar (lambda (x) (if (listp x) (cadr x) t)) specialized)))))

(defmacro %defmethod-expander
    (name qualifiers lambda-list body &environment env)
  (multiple-value-bind (proto-gf proto-method)
      (prototypes-for-make-method-lambda name)
    (expand-defmethod name proto-gf proto-method qualifiers
                      lambda-list body env)))


(defun prototypes-for-make-method-lambda (name)
  (if (not (eq **boot-state** 'complete))
      (values nil nil)
      (let ((gf? (and (fboundp name)
                      (gdefinition name))))
        (if (or (null gf?)
                (not (generic-function-p gf?)))
            (values (class-prototype (find-class 'standard-generic-function))
                    (class-prototype (find-class 'standard-method)))
            (values gf?
                    (class-prototype (or (generic-function-method-class gf?)
                                         (find-class 'standard-method))))))))

;;; These are used to communicate the method name and lambda-list to
;;; MAKE-METHOD-LAMBDA-INTERNAL.
(defvar *method-name* nil)
(defvar *method-lambda-list* nil)

(defun expand-defmethod (name proto-gf proto-method qualifiers lambda-list
                         body env)
  (binding* (;; ENV could be of type SB!INTERPRETER:BASIC-ENV but I
             ;; don't care to figure out what parts of PCL would have
             ;; to change to accept that, so coerce.
             (env (sb!kernel:coerce-to-lexenv env))
             ((nil unspecialized-lambda-list specializers)
              (parse-specialized-lambda-list lambda-list))
             (*method-name* `(,name ,@qualifiers ,specializers))
             (method-lambda `(lambda ,unspecialized-lambda-list ,@body))
             ((method-function-lambda initargs new-lambda-list)
              (make-method-lambda-using-specializers
               proto-gf proto-method qualifiers specializers method-lambda env))
             (initargs-form
              (make-method-initargs-form
               proto-gf proto-method method-function-lambda initargs env))
             (specializers-form
              (make-method-specializers-form
               proto-gf proto-method specializers env)))
    (mapc (lambda (specializer parameter)
            (when (typep specializer 'type-specifier)
              (with-current-source-form (parameter)
                (check-deprecated-type specializer))))
          specializers lambda-list)
    ;; Note: We could DECLAIM the ftype of the generic function here,
    ;; since ANSI specifies that we create it if it does not
    ;; exist. However, I chose not to, because I think it's more
    ;; useful to support a style of programming where every generic
    ;; function has an explicit DEFGENERIC and any typos in DEFMETHODs
    ;; are warned about. Otherwise
    ;;
    ;;   (DEFGENERIC FOO-BAR-BLETCH (X))
    ;;   (DEFMETHOD FOO-BAR-BLETCH ((X HASH-TABLE)) ..)
    ;;   (DEFMETHOD FOO-BRA-BLETCH ((X SIMPLE-VECTOR)) ..)
    ;;   (DEFMETHOD FOO-BAR-BLETCH ((X VECTOR)) ..)
    ;;   (DEFMETHOD FOO-BAR-BLETCH ((X ARRAY)) ..)
    ;;   (DEFMETHOD FOO-BAR-BLETCH ((X LIST)) ..)
    ;;
    ;; compiles without raising an error and runs without raising an
    ;; error (since SIMPLE-VECTOR cases fall through to VECTOR) but
    ;; still doesn't do what was intended. I hate that kind of bug
    ;; (code which silently gives the wrong answer), so we don't do a
    ;; DECLAIM here. -- WHN 20000229
    (make-defmethod-form name qualifiers specializers-form
                         (or new-lambda-list unspecialized-lambda-list)
                         (if proto-method
                             (class-name (class-of proto-method))
                             'standard-method)
                         initargs-form)))

(defun interned-symbol-p (x)
  (and (symbolp x) (symbol-package x)))

(defun make-defmethod-form
    (name qualifiers specializers unspecialized-lambda-list
     method-class-name initargs-form)
  #+nil
  (declare (sb!ext:muffle-conditions sb!ext:code-deletion-note))
  (let (fn
        fn-lambda)
    (if (and (interned-symbol-p (fun-name-block-name name))
             (every #'interned-symbol-p qualifiers)
             (every (lambda (s)
                      (if (consp s)
                          (and (eq (car s) 'eql)
                               (sb-xc:constantp (cadr s))
                               (let ((sv (sb!impl::constant-form-value (cadr s))))
                                 (or (interned-symbol-p sv)
                                     (integerp sv)
                                     (and (characterp sv)
                                          (standard-char-p sv)))))
                          (interned-symbol-p s)))
                    specializers)
             (consp initargs-form)
             (eq (car initargs-form) 'list*)
             (memq (cadr initargs-form) '(:function))
             (consp (setq fn (caddr initargs-form)))
             (eq (car fn) 'function)
             (consp (setq fn-lambda (cadr fn)))
             (eq (car fn-lambda) 'lambda)
             (bug "Really got here"))
        (let* ((specls (mapcar (lambda (specl)
                                 (if (consp specl)
                                     ;; CONSTANT-FORM-VALUE?  What I
                                     ;; kind of want to know, though,
                                     ;; is what happens if we don't do
                                     ;; this for some slow-method
                                     ;; function because of a hairy
                                     ;; lexenv -- is the only bad
                                     ;; effect that the method
                                     ;; function ends up unnamed?  If
                                     ;; so, couldn't we arrange to
                                     ;; name it later?
                                     `(,(car specl) ,(eval (cadr specl)))
                                   specl))
                               specializers))
               (mname `(,(if (eq (cadr initargs-form) :function)
                             'slow-method 'fast-method)
                        ,name ,@qualifiers ,specls)))
          `(progn
             (defun ,mname ,(cadr fn-lambda)
               ,@(cddr fn-lambda))
             ,(make-defmethod-form-internal
               name qualifiers `',specls
               unspecialized-lambda-list method-class-name
               `(list* ,(cadr initargs-form)
                       #',mname
                       ,@(cdddr initargs-form)))))
        (make-defmethod-form-internal
         name qualifiers
         specializers
         #+nil
         `(list ,@(mapcar (lambda (specializer)
                            (if (consp specializer)
                                ``(,',(car specializer)
                                      ,,(cadr specializer))
                                `',specializer))
                          specializers))
         unspecialized-lambda-list
         method-class-name
         initargs-form))))

(defun make-defmethod-form-internal
    (name qualifiers specializers-form unspecialized-lambda-list
     method-class-name initargs-form)
  `(load-defmethod
    ',method-class-name
    ',name
    ',qualifiers
    ,specializers-form
    ',unspecialized-lambda-list
    ,initargs-form
    (sb!c:source-location)))

(defmacro make-method-function (method-lambda &environment env)
  (binding* (((proto-gf proto-method)
              (prototypes-for-make-method-lambda nil))
             ((method-function-lambda initargs)
              (make-method-lambda proto-gf proto-method method-lambda env))) ; FIXME: coerce-to-lexenv?
    (make-method-initargs-form
     proto-gf proto-method method-function-lambda initargs env)))

(defun real-make-method-initargs-form (proto-gf proto-method
                                       method-lambda initargs env)
  (declare (ignore proto-gf proto-method))
  (check-method-lambda method-lambda 'make-method-initargs)
  (make-method-initargs-form-internal method-lambda initargs env))

(unless (fboundp 'make-method-initargs-form)
  (setf (gdefinition 'make-method-initargs-form)
        (symbol-function 'real-make-method-initargs-form)))

(defun real-make-method-lambda-using-specializers
    (proto-gf proto-method qualifiers specializers method-lambda env)
  (declare (ignore qualifiers))
  (check-method-lambda method-lambda 'make-method-lambda) ; TODO remove check in make-method-lambda
  ;; Default behavior: delegate to MAKE-METHOD-LAMBDA.
  (let* ((lambda-list (second method-lambda))
         (*method-lambda-list*
          (append
           (mapcar #'list (subseq lambda-list 0 (length specializers)) specializers)
           (subseq lambda-list (length specializers)))))
    (make-method-lambda proto-gf proto-method method-lambda env)))

(unless (fboundp 'make-method-lambda-using-specializers)
  (setf (gdefinition 'make-method-lambda-using-specializers)
        (symbol-function 'real-make-method-lambda-using-specializers)))

;;; When bootstrapping PCL MAKE-METHOD-LAMBDA starts out as a regular
;;; function: REAL-MAKE-METHOD-LAMBDA set to the fdefinition of
;;; MAKE-METHOD-LAMBDA. Once generic functions are born,
;;; REAL-MAKE-METHOD-LAMBDA is used to implement the default method.
;;; MAKE-METHOD-LAMBDA-INTERNAL is split out into a separate function
;;; so that changing it in a live image is easy, and changes actually
;;; take effect.
(defun real-make-method-lambda (proto-gf proto-method method-lambda env)
  (make-method-lambda-internal proto-gf proto-method method-lambda env))

(unless (fboundp 'make-method-lambda)
  (setf (gdefinition 'make-method-lambda)
        (symbol-function 'real-make-method-lambda)))


;;; A helper function for creating Python-friendly type declarations
;;; in DEFMETHOD forms.
;;;
;;; This function operates on
;;; * non-parsed specializers, i.e. class names and extended
;;;   specializer syntaxes
;;; * parsed specializers, i.e. CLASSes, EQL-SPECIALIZERs,
;;;   CLASS-EQ-SPECIALIZERs and generic SPECIALIZERs
;;;
;;; We're too lazy to cons up a new environment for this, so we just
;;; pass in the list of locally declared specials in addition to the
;;; old environment.
(defun parameter-specializer-declaration-in-defmethod
    (proto-generic-function proto-method parameter specializer specials env)
  (flet ((declare-type (type)
           (return-from parameter-specializer-declaration-in-defmethod
             (case type
               ((nil) '(ignorable))
               (t     `(type ,type ,parameter))))))
    (cond
      ((not (eq **boot-state** 'complete))
       ;; KLUDGE: PCL, in its wisdom, sometimes calls methods with
       ;; types which don't match their specializers. (Specifically,
       ;; it calls ENSURE-CLASS-USING-CLASS (T NULL) with a non-NULL
       ;; second argument.) Hopefully it only does this kind of
       ;; weirdness when bootstrapping.. -- WHN 20000610
       (declare-type nil))

      ;; Independent of SPECIALIZER, bail out if the PARAMETER is
      ;; known to be a special variable. Our rebinding magic for SETQ
      ;; cases doesn't work right there as SET, (SETF SYMBOL-VALUE),
      ;; etc. make things undecidable.
      ((or (var-special-p parameter env) (member parameter specials))
       (declare-type nil))

      ;; Bail out on SLOT-OBJECT special case.
      ;;
      ;; KLUDGE: For some low-level implementation classes, perhaps
      ;; because of some problems related to the incomplete
      ;; integration of PCL into SBCL's type system, some specializer
      ;; classes can't be declared as argument types. E.g.
      ;;   (DEFMETHOD FOO ((X SLOT-OBJECT))
      ;;     (DECLARE (TYPE SLOT-OBJECT X))
      ;;     ..)
      ;; loses when
      ;;   (DEFSTRUCT BAR A B)
      ;;   (FOO (MAKE-BAR))
      ;; perhaps because of the way that STRUCTURE-OBJECT inherits
      ;; both from SLOT-OBJECT and from SB!KERNEL:INSTANCE. In an
      ;; effort to sweep such problems under the rug, we exclude these
      ;; problem cases by blacklisting them here. -- WHN 2001-01-19
      ((eq specializer 'slot-object)
       (declare-type nil))

      ;; Bail out on  unparsed EQL-specializers.
      ;;
      ;; KLUDGE: ANSI, in its wisdom, says that EQL-SPECIALIZER-FORMs
      ;; in EQL specializers are evaluated at DEFMETHOD expansion
      ;; time. Thus, although one might think that in
      ;;   (DEFMETHOD FOO ((X PACKAGE)
      ;;                   (Y (EQL 12))
      ;;      ..))
      ;; the PACKAGE and (EQL 12) forms are both parallel type names,
      ;; they're not, as is made clear when you do
      ;;   (DEFMETHOD FOO ((X PACKAGE)
      ;;                   (Y (EQL 'BAR)))
      ;;     ..)
      ;; where Y needs to be a symbol named "BAR", not some cons made
      ;; by (CONS 'QUOTE 'BAR). I.e. when the EQL-SPECIALIZER-FORM is
      ;; (EQL 'X), it requires an argument to be of type (EQL X). It'd
      ;; be easy to transform one to the other, but it'd be somewhat
      ;; messier to do so while ensuring that the EQL-SPECIALIZER-FORM
      ;; is only EVAL'd once. (The new code wouldn't be messy, but
      ;; it'd require a big transformation of the old code.) So
      ;; instead we punt.  -- WHN 20000610
      ((typep specializer '(cons (eql eql)))
       (declare-type nil))

      ;; Parsed specializer objects, i.e. CLASS, EQL-SPECIALIZER,
      ;; CLASS-EQ-SPECIALIZER and generic SPECIALIZER.
      ;;
      ;; Also unparsed specializers other than EQL: these have to be
      ;; either class names or extended specializers.
      ;;
      ;; For these, we can usually make Python very happy.
      ;;
      ;; KLUDGE: Since INFO doesn't work right for class objects here,
      ;; and they are valid specializers, see if the specializer is
      ;; a named class, and use the name in that case -- otherwise
      ;; the class instance is ok, since info will just return NIL, NIL.
      ;;
      ;; We still need to deal with the class case too, but at
      ;; least #.(find-class 'integer) and integer as equivalent
      ;; specializers with this.
      (t
       (declare-type (specializer-type-specifier
                      proto-generic-function proto-method specializer))))))

(defun make-method-lambda-internal (proto-gf proto-method method-lambda env)
  (check-method-lambda method-lambda 'make-method-lambda)

  (binding* (((real-body declarations documentation)
              (parse-body (cddr method-lambda) t))
             ;; We have the %METHOD-NAME declaration in the place
             ;; where we expect it only if there is are no
             ;; non-standard prior MAKE-METHOD-LAMBDA methods -- or
             ;; unless they're fantastically unintrusive.
             (method-name *method-name*)
             (method-lambda-list *method-lambda-list*)
             ;; Macroexpansion caused by code-walking may call
             ;; make-method-lambda and end up with wrong values
             (*method-name* nil)
             (*method-lambda-list* nil)
             (generic-function-name (when method-name (car method-name)))
             ;; the method-cell is a way of communicating what method
             ;; a method-function implements, for the purpose of
             ;; NO-NEXT-METHOD.  We need something that can be shared
             ;; between function and initargs, but not something that
             ;; will be coalesced as a constant (because we are
             ;; naughty, oh yes) with the expansion of any other
             ;; methods in the same file.  -- CSR, 2007-05-30
             (method-cell (list (make-symbol "METHOD-CELL")))
             ((parameters lambda-list specializers)
              (parse-specialized-lambda-list
               (or method-lambda-list
                   (ecase (car method-lambda)
                     (lambda (second method-lambda))
                     (named-lambda (third method-lambda))))))
             (required-parameters (subseq parameters 0 (length specializers)))
             (slots (mapcar #'list required-parameters))
             (class-declarations
              `(declare
                ;; These declarations seem to be used by PCL to pass
                ;; information to itself; when I tried to delete 'em
                ;; ca. 0.6.10 it didn't work. I'm not sure how they
                ;; work, but note the (VAR-DECLARATION '%CLASS ..)
                ;; expression in CAN-OPTIMIZE-ACCESS1. -- WHN
                ;; 2000-12-30
                ,@(mapcan (lambda (parameter specializer)
                            (when (typep specializer '(and symbol (not (eql t))))
                              (list `(%class ,parameter ,specializer))))
                          parameters specializers)
                ;; These TYPE declarations weren't in the original PCL
                ;; code, but the Python compiler likes them a
                ;; lot. (We're telling the compiler about our
                ;; knowledge of specialized argument types so that it
                ;; can avoid run-time type dispatch overhead, which
                ;; can be a huge win for Python.)
                ,@(let ((specials (declared-specials declarations)))
                    (mapcar (lambda (par spec)
                              (parameter-specializer-declaration-in-defmethod
                               proto-gf proto-method par spec specials env))
                            parameters specializers))))
             (method-lambda
              ;; Remove the documentation string and insert the
              ;; appropriate class declarations. The documentation
              ;; string is removed to make it easy for us to insert
              ;; new declarations later, they will just go after the
              ;; CADR of the method lambda. The class declarations
              ;; are inserted to communicate the class of the method's
              ;; arguments to the code walk.
              `(lambda ,lambda-list
                 ;; The default ignorability of method parameters
                 ;; doesn't seem to be specified by ANSI. PCL had
                 ;; them basically ignorable but was a little
                 ;; inconsistent. E.g. even though the two
                 ;; method definitions
                 ;;   (DEFMETHOD FOO ((X T) (Y T)) "Z")
                 ;;   (DEFMETHOD FOO ((X T) Y) "Z")
                 ;; are otherwise equivalent, PCL treated Y as
                 ;; ignorable in the first definition but not in the
                 ;; second definition. We make all required
                 ;; parameters ignorable as a way of systematizing
                 ;; the old PCL behavior. -- WHN 2000-11-24
                 (declare (ignorable ,@required-parameters))
                 ,class-declarations
                 ,@declarations
                 (block ,(fun-name-block-name generic-function-name)
                   ,@real-body)))
             (constant-value-p (and (null (cdr real-body))
                                    (sb-xc:constantp (car real-body))))
             (constant-value (when constant-value-p
                               (sb!impl::constant-form-value (car real-body))))
             (plist (when (and constant-value-p
                               (or (typep constant-value '(or number character))
                                   (and (symbolp constant-value)
                                        (symbol-package constant-value))))
                         (list :constant-value constant-value)))
             (applyp (dolist (p lambda-list nil)
                       (cond ((memq p '(&optional &rest &key))
                              (return t))
                             ((eq p '&aux)
                              (return nil)))))
             ((walked-lambda call-next-method-p setq-p parameters-setqd)
              (walk-method-lambda
               method-lambda required-parameters env slots))
             ((walked-lambda-body walked-declarations)
              (parse-body (cddr walked-lambda) t)))
    (when (some #'cdr slots)
      (let ((slot-name-lists (slot-name-lists-from-slots slots)))
        (setf plist
              `(,@(when slot-name-lists
                    `(:slot-name-lists ,slot-name-lists))
                  ,@plist)
              walked-lambda-body
              `((pv-binding (,required-parameters
                             ,slot-name-lists
                             (load-time-value
                              (intern-pv-table
                               :slot-name-lists ',slot-name-lists)))
                  ,@walked-lambda-body)))))
    (when (and (memq '&key lambda-list)
               (not (memq '&allow-other-keys lambda-list)))
      (let ((aux (memq '&aux lambda-list)))
        (setq lambda-list (nconc (ldiff lambda-list aux)
                                 (list '&allow-other-keys)
                                 aux))))
    (values `(lambda (.method-args. .next-methods.)
               (simple-lexical-method-functions
                   (,lambda-list .method-args. .next-methods.
                                 :call-next-method-p
                                 ,(when call-next-method-p t)
                                 :setq-p ,setq-p
                                 :parameters-setqd ,parameters-setqd
                                 :method-cell ,method-cell
                                 :applyp ,applyp)
                 ,@walked-declarations
                 (locally (declare (disable-package-locks
                                    %parameter-binding-modified))
                   (symbol-macrolet ((%parameter-binding-modified
                                      ',@parameters-setqd))
                     (declare (enable-package-locks
                               %parameter-binding-modified))
                     ,@walked-lambda-body))))
            `(,@(when call-next-method-p `(method-cell ,method-cell))
              ,@(when (member call-next-method-p '(:simple nil))
                  '(simple-next-method-call t))
              ,@(when plist `(plist ,plist))
              ,@(when documentation `(:documentation ,documentation))))))

(defun declared-specials (declarations)
  (loop for (declare . specifiers) in declarations
        append (loop for specifier in specifiers
                     when (eq 'special (car specifier))
                     append (cdr specifier))))

(defun real-make-method-specializers-form
    (proto-generic-function proto-method specializer-names environment)
  (flet ((make-parse-form (name)
           (make-specializer-form-using-class
            proto-generic-function proto-method name environment)))
    `(list ,@(mapcar #'make-parse-form specializer-names))))

(unless (fboundp 'make-method-specializers-form)
  (setf (gdefinition 'make-method-specializers-form)
        (symbol-function 'real-make-method-specializers-form)))

(defun real-make-specializer-form-using-class/t
    (proto-generic-function proto-method specializer-name environment)
  (declare (ignore proto-generic-function proto-method environment))
  (error 'simple-reference-error
         :format-control
         "~@<~S is not a valid parameter specializer name.~@:>"
         :format-arguments (list specializer-name)
         :references (list '(:ansi-cl :macro defmethod)
                           '(:ansi-cl :glossary "parameter specializer name"))))

(defun real-make-specializer-form-using-class/specializer
    (proto-generic-function proto-method specializer-name environment)
  (declare (ignore proto-generic-function proto-method environment))
  (when (eq **boot-state** 'complete)
    specializer-name))

(defun real-make-specializer-form-using-class/symbol
    (proto-generic-function proto-method specializer-name environment)
  (declare (ignore proto-generic-function proto-method environment))
  `(find-class ',specializer-name))

(defun real-make-specializer-form-using-class/cons
    (proto-generic-function proto-method specializer-name environment)
  (declare (ignore proto-generic-function proto-method environment))
  ;; In case of unknown specializer or known specializer with syntax
  ;; error, TYPECASE may fall through to default method with error
  ;; signaling.
  (typecase specializer-name
    ((cons (eql eql) (cons t null))
     `(intern-eql-specializer ,(second specializer-name)))
    ((cons (eql class-eq) (cons t null))
     `(class-eq-specializer (find-class ',(second specializer-name))))))

(defun real-make-specializer-form-using-class
    (proto-generic-function proto-method specializer-name environment)
  (macrolet
      ((delegations ()
         `(typecase specializer-name
            ,@(mapcar
               (lambda (type)
                 (let ((function-name
                         (symbolicate
                          'real-make-specializer-form-using-class '#:/ type)))
                   `(,type
                     (,function-name
                      proto-generic-function proto-method specializer-name environment))))
               '(; specializer
                 ; ^ apparently not needed during bootstrapping
                 symbol cons t)))))
    (delegations)))

(unless (fboundp 'make-specializer-form-using-class)
  (setf (gdefinition 'make-specializer-form-using-class)
        (symbol-function 'real-make-specializer-form-using-class)))

(defun real-specializer-type-specifier/specializer
    (proto-generic-function proto-method specializer)
  (declare (ignore proto-generic-function proto-method))
  ;; TODO later protocol-unimplemented-error?
  (style-warn "~@<No method on ~S for specializer ~S~@:>"
              'specializer-type-specifier specializer)
  nil)

(labels ((warn-parse (specializer &optional condition)
           (style-warn
            "~@<Cannot parse specializer ~S in ~S~@[: ~A~].~@:>"
            specializer 'specializer-type-specifier condition))
         (warn-find (condition name proto-generic-function proto-method)
           (warn condition
                 :format-control
                 "~@<Cannot find type for specializer ~
                  ~/sb!ext:print-symbol-with-prefix/ when executing ~S ~
                  for a ~/sb!ext:print-type-specifier/ of a ~
                  ~/sb!ext:print-type-specifier/.~@:>"
                 :format-arguments
                 (list name 'specializer-type-specifier
                       (class-name (class-of proto-method))
                       (class-name (class-of proto-generic-function)))))
         (class-name-type-specifier (name proto-generic-function proto-method)
           (let ((kind (info :type :kind name)))
             (case kind
               (:primitive
                name)
               (:defined
                ;; This can happen if NAME is a DEFTYPE.
                (warn-find 'simple-warning
                           name proto-generic-function proto-method))
               ((:instance :forthcoming-defclass-type)
                ;; CLOS classes are too expensive to check (as opposed
                ;; to STRUCTURE-CLASS and SYSTEM-CLASS).
                nil)
               (t
                ;; TODO proper warning condition?
                (warn-find 'simple-style-warning
                           name proto-generic-function proto-method)
                nil)))))

  ;;; Non-parsed class specializers, i.e. class names
  ;;;
  ;;; Extended generic function classes with specializers which are
  ;;; designated by symbols have to install their own methods
  ;;; specialized on symbol to replace this logic.

  (defun real-specializer-type-specifier/symbol
      (proto-generic-function proto-method specializer)
    (let ((specializer
           (handler-case
               ;; Usually tries to find the class named
               ;; SPECIALIZER. Can do something different when there
               ;; is a non-default method on
               ;; PARSE-SPECIALIZER-USING-CLASS.
               (parse-specializer-using-class
                proto-generic-function specializer)
             (class-not-found-error ()
               ;; SPECIALIZER does not name a class, but maybe it is
               ;; known to name a :forthcoming-defclass-type.
               ;; CLASS-NAME-TYPE-SPECIFIER will emit the warning and
               ;; return nil if not.
               (class-name-type-specifier
                specializer proto-generic-function proto-method))
             (error (condition)
               ;; This can only happen if there is an EQL-specialized
               ;; method on PARSE-SPECIALIZER-USING-CLASS matching
               ;; SPECIALIZER that signals an error.
               (warn-parse specializer condition)
               nil))))
      (when specializer
        (specializer-type-specifier
         proto-generic-function proto-method specializer))))

  ;;; Non-parsed extended specializer with default syntax
  ;;; i.e. (SPECIALIZER-KIND &rest SPECIFIC-SYNTAX)

  (defun real-specializer-type-specifier/t
      (proto-generic-function proto-method specializer)
    (let ((specializer
           (handler-case
               (parse-specializer-using-class
                proto-generic-function specializer)
             (error (condition)
               ;; This can happen, for example, if SPECIALIZER does
               ;; not designate any extended specializer or if it does
               ;; but then does not conform to the respective extended
               ;; specializer syntax.
               (warn-parse specializer condition)
               nil))))
      (when specializer
        (specializer-type-specifier
         proto-generic-function proto-method specializer))))

  ;;; Parsed EQL and CLASS-EQ specializers

  (defun real-specializer-type-specifier/class-eq-specializer
      (proto-generic-function proto-method specializer)
    (specializer-type-specifier
     proto-generic-function proto-method (specializer-class specializer)))

  (defun real-specializer-type-specifier/eql-specializer
      (proto-generic-function proto-method specializer)
    (declare (ignore proto-generic-function proto-method))
    `(eql ,(eql-specializer-object specializer)))

  ;;; Parsed class specializers

  (defun real-specializer-type-specifier/structure-class
      (proto-generic-function proto-method specializer)
    (declare (ignore proto-generic-function proto-method))
    (class-name specializer))

  (defun real-specializer-type-specifier/system-class
      (proto-generic-function proto-method specializer)
    (declare (ignore proto-generic-function proto-method))
    (class-name specializer))

  (defun real-specializer-type-specifier/class
      (proto-generic-function proto-method specializer)
    (let ((name (class-name specializer)))
      ;; Make sure SPECIALIZER has a proper class name and that name
      ;; designates the class SPECIALIZER in the global environment.
      (when (and (typep name '(and symbol (not null)))
                 (eq specializer (find-class name nil)))
        (class-name-type-specifier
         name proto-generic-function proto-method)))))

(defun real-specializer-type-specifier
    (proto-generic-function proto-method specializer)
  (macrolet
      ((delegations ()
         `(typecase specializer
            ,@(mapcar
               (lambda (type)
                 (let ((function-name
                         (symbolicate
                          'real-specializer-type-specifier '#:/ type)))
                   `(,type
                     (,function-name
                      proto-generic-function proto-method specializer))))
               '(specializer symbol t class-eq-specializer eql-specializer
                 structure-class system-class class)))))
    (delegations)))

(unless (fboundp 'specializer-type-specifier)
  (setf (gdefinition 'specializer-type-specifier)
        (symbol-function 'real-specializer-type-specifier)))

(defun real-parse-specializer-using-class (generic-function specializer)
  (let ((result (specializer-from-type specializer)))
    (if (specializerp result)
        result
        (error "~@<~S cannot be parsed as a specializer for ~S.~@:>"
               specializer generic-function))))

(unless (fboundp 'parse-specializer-using-class)
  (setf (gdefinition 'parse-specializer-using-class)
        (symbol-function 'real-parse-specializer-using-class)))

(defun real-unparse-specializer-using-class (generic-function specializer)
  (if (specializerp specializer)
      ;; FIXME: this HANDLER-CASE is a bit of a hammer to crack a nut:
      ;; the idea is that we want to unparse permissively, so that the
      ;; lazy (or rather the "portable") specializer extender (who
      ;; does not define methods on these new SBCL-specific MOP
      ;; functions) can still subclass specializer and define methods
      ;; without everything going wrong.  Making it cleaner and
      ;; clearer that that is what we are defending against would be
      ;; nice.  -- CSR, 2007-06-01
      (handler-case
          (let ((type (specializer-type specializer)))
            (if (and (consp type) (eq (car type) 'class))
                (let* ((class (cadr type))
                       (class-name (class-name class)))
                  (if (eq class (find-class class-name nil))
                      class-name
                      type))
                type))
        (error () specializer))
      (error "~@<~S is not a legal specializer for ~S.~@:>"
             specializer generic-function)))

(unless (fboundp 'unparse-specializer-using-class)
  (setf (gdefinition 'unparse-specializer-using-class)
        (symbol-function 'real-unparse-specializer-using-class)))

;;; For passing a list (groveled by the walker) of the required
;;; parameters whose bindings are modified in the method body to the
;;; optimized-slot-value* macros.
(define-symbol-macro %parameter-binding-modified ())

(defmacro simple-lexical-method-functions ((lambda-list
                                            method-args
                                            next-methods
                                            &rest lmf-options)
                                           &body body)
  `(progn
     ,method-args ,next-methods
     (bind-simple-lexical-method-functions (,method-args ,next-methods
                                                         ,lmf-options)
         (bind-args (,lambda-list ,method-args)
           ,@body))))

(defmacro fast-lexical-method-functions ((lambda-list
                                          next-method-call
                                          args
                                          rest-arg
                                          &rest lmf-options)
                                         &body body)
  `(bind-fast-lexical-method-functions (,args ,rest-arg ,next-method-call ,lmf-options)
     (bind-args (,(nthcdr (length args) lambda-list) ,rest-arg)
       ,@body)))

(defmacro bind-simple-lexical-method-functions
    ((method-args next-methods (&key call-next-method-p setq-p
                                     parameters-setqd applyp method-cell))
     &body body
     &environment env)
  (declare (ignore parameters-setqd))
  (if (not (or call-next-method-p setq-p applyp))
      ;; always provide the lexical function NEXT-METHOD-P.
      ;; I would think this to be a good candidate for declaring INLINE
      ;; but that's not the way it was done before.
      `(flet ((next-method-p () (not (null (car ,next-methods)))))
         (declare (ignorable #'next-method-p))
         ,@body)
      `(let ((.next-method. (car ,next-methods))
             (,next-methods (cdr ,next-methods)))
         (declare (ignorable .next-method. ,next-methods))
         (flet (,@(when call-next-method-p
                    `((call-next-method (&rest cnm-args)
                       (declare (dynamic-extent cnm-args))
                       ,@(if (safe-code-p env)
                             `((%check-cnm-args cnm-args
                                                ,method-args
                                                ',method-cell))
                             nil)
                       (if .next-method.
                           (funcall (if (std-instance-p .next-method.)
                                        (method-function .next-method.)
                                        .next-method.) ; for early methods
                                    (or cnm-args ,method-args)
                                    ,next-methods)
                           (apply #'call-no-next-method
                                  ',method-cell
                                  (or cnm-args ,method-args))))))
                (next-method-p () (not (null .next-method.))))
           (declare (ignorable #'next-method-p))
           ,@body))))

(defun call-no-next-method (method-cell &rest args)
  (let ((method (car method-cell)))
    (aver method)
    ;; Can't easily provide a RETRY restart here, as the return value here is
    ;; for the method, not the generic function.
    (apply #'no-next-method (method-generic-function method)
           method args)))

(defun call-no-applicable-method (gf args)
  (restart-case
          (apply #'no-applicable-method gf args)
    (retry ()
      :report "Retry calling the generic function."
      (apply gf args))))

(defun call-no-primary-method (gf args)
  (restart-case
      (apply #'no-primary-method gf args)
    (retry ()
      :report "Retry calling the generic function."
      (apply gf args))))

(defstruct (method-call (:copier nil))
  (function #'identity :type function)
  call-method-args)
(defstruct (constant-method-call (:copier nil) (:include method-call))
  value)

#!-sb-fluid (declaim (sb!ext:freeze-type method-call))

(defmacro invoke-method-call1 (function args cm-args)
  `(let ((.function. ,function)
         (.args. ,args)
         (.cm-args. ,cm-args))
     (if (and .cm-args. (null (cdr .cm-args.)))
         (funcall .function. .args. (car .cm-args.))
         (apply .function. .args. .cm-args.))))

(defmacro invoke-method-call (method-call restp &rest required-args+rest-arg)
  `(invoke-method-call1 (method-call-function ,method-call)
                        ,(if restp
                             `(list* ,@required-args+rest-arg)
                             `(list ,@required-args+rest-arg))
                        (method-call-call-method-args ,method-call)))

(defstruct (fast-method-call (:copier nil))
  (function #'identity :type function)
  pv
  next-method-call
  arg-info)
(defstruct (constant-fast-method-call
             (:copier nil) (:include fast-method-call))
  value)

#!-sb-fluid (declaim (sb!ext:freeze-type fast-method-call))

;; The two variants of INVOKE-FAST-METHOD-CALL differ in how REST-ARGs
;; are handled. The first one will get REST-ARG as a single list (as
;; the last argument), and will thus need to use APPLY. The second one
;; will get them as a &MORE argument, so we can pass the arguments
;; directly with MULTIPLE-VALUE-CALL and %MORE-ARG-VALUES.


(defmacro invoke-fast-method-call (method-call restp &rest required-args+rest-arg)
  `(,(if restp 'apply 'funcall) (fast-method-call-function ,method-call)
                                (fast-method-call-pv ,method-call)
                                (fast-method-call-next-method-call ,method-call)
                                ,@required-args+rest-arg))

(defmacro invoke-fast-method-call/more (method-call
                                        more-context
                                        more-count
                                        &rest required-args)
  (macrolet ((generate-call (n)
               ``(funcall (fast-method-call-function ,method-call)
                          (fast-method-call-pv ,method-call)
                          (fast-method-call-next-method-call ,method-call)
                          ,@required-args
                          ,@(loop for x below ,n
                                  collect `(sb!c::%more-arg ,more-context ,x)))))
    ;; The cases with only small amounts of required arguments passed
    ;; are probably very common, and special-casing speeds them up by
    ;; a factor of 2 with very little effect on the other
    ;; cases. Though it'd be nice to have the generic case be equally
    ;; fast.
    `(case ,more-count
       (0 ,(generate-call 0))
       (1 ,(generate-call 1))
       (t (multiple-value-call (fast-method-call-function ,method-call)
            (values (fast-method-call-pv ,method-call))
            (values (fast-method-call-next-method-call ,method-call))
            ,@required-args
            (sb!c::%more-arg-values ,more-context 0 ,more-count))))))

(defstruct (fast-instance-boundp (:copier nil))
  (index 0 :type fixnum))

#!-sb-fluid (declaim (sb!ext:freeze-type fast-instance-boundp))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *allow-emf-call-tracing-p* nil)
  (defvar *enable-emf-call-tracing-p* #!-sb-show nil #!+sb-show t))

;;;; effective method functions

(defvar *emf-call-trace-size* 200)
(defvar *emf-call-trace* nil)
(defvar *emf-call-trace-index* 0)

;;; This function was in the CMU CL version of PCL (ca Debian 2.4.8)
;;; without explanation. It appears to be intended for debugging, so
;;; it might be useful someday, so I haven't deleted it.
;;; But it isn't documented and isn't used for anything now, so
;;; I've conditionalized it out of the base system. -- WHN 19991213
#!+sb-show
(defun show-emf-call-trace ()
  (when *emf-call-trace*
    (let ((j *emf-call-trace-index*)
          (*enable-emf-call-tracing-p* nil))
      (format t "~&(The oldest entries are printed first)~%")
      (dotimes-fixnum (i *emf-call-trace-size*)
        (let ((ct (aref *emf-call-trace* j)))
          (when ct (print ct)))
        (incf j)
        (when (= j *emf-call-trace-size*)
          (setq j 0))))))

(defun trace-emf-call-internal (emf format args)
  (unless *emf-call-trace*
    (setq *emf-call-trace* (make-array *emf-call-trace-size*)))
  (setf (aref *emf-call-trace* *emf-call-trace-index*)
        (list* emf format args))
  (incf *emf-call-trace-index*)
  (when (= *emf-call-trace-index* *emf-call-trace-size*)
    (setq *emf-call-trace-index* 0)))

(defmacro trace-emf-call (emf format args)
  (when *allow-emf-call-tracing-p*
    `(when *enable-emf-call-tracing-p*
       (trace-emf-call-internal ,emf ,format ,args))))

(defmacro invoke-effective-method-function-fast
    (emf restp &key required-args rest-arg more-arg)
  `(progn
     (trace-emf-call ,emf ,restp (list ,@required-args rest-arg))
     ,(if more-arg
          `(invoke-fast-method-call/more ,emf
                                         ,@more-arg
                                         ,@required-args)
          `(invoke-fast-method-call ,emf
                                    ,restp
                                    ,@required-args
                                    ,@rest-arg))))

(defun effective-method-optimized-slot-access-clause
    (emf restp required-args)
  ;; "What," you may wonder, "do these next two clauses do?" In that
  ;; case, you are not a PCL implementor, for they considered this to
  ;; be self-documenting.:-| Or CSR, for that matter, since he can
  ;; also figure it out by looking at it without breaking stride. For
  ;; the rest of us, though: From what the code is doing with .SLOTS.
  ;; and whatnot, evidently it's implementing SLOT-VALUEish and
  ;; GET-SLOT-VALUEish things. Then we can reason backwards and
  ;; conclude that setting EMF to a FIXNUM is an optimized way to
  ;; represent these slot access operations.
  (when (not restp)
    (let ((length (length required-args)))
      (cond ((= 1 length)
             `((fixnum
                (let* ((.slots. (get-slots-or-nil
                                 ,(car required-args)))
                       (value (when .slots. (clos-slots-ref .slots. ,emf))))
                  (if (eq value +slot-unbound+)
                      (slot-unbound-internal ,(car required-args)
                                             ,emf)
                      value)))))
            ((= 2 length)
             `((fixnum
                (let ((.new-value. ,(car required-args))
                      (.slots. (get-slots-or-nil
                                ,(cadr required-args))))
                  (when .slots.
                    (setf (clos-slots-ref .slots. ,emf) .new-value.)))))))
      ;; (In cmucl-2.4.8 there was a commented-out third ,@(WHEN
      ;; ...) clause here to handle SLOT-BOUNDish stuff. Since
      ;; there was no explanation and presumably the code is 10+
      ;; years stale, I simply deleted it. -- WHN)
      )))

;;; Before SBCL 0.9.16.7 instead of
;;; INVOKE-NARROW-EFFECTIVE-METHOD-FUNCTION we passed a (THE (OR
;;; FUNCTION METHOD-CALL FAST-METHOD-CALL) EMF) form as the EMF. Now,
;;; to make less work for the compiler we take a path that doesn't
;;; involve the slot-accessor clause (where EMF is a FIXNUM) at all.
(macrolet ((def (name &optional narrow)
             `(defmacro ,name (emf restp &key required-args rest-arg more-arg)
                (unless (sb-xc:constantp restp)
                  (error "The RESTP argument is not constant."))
                (setq restp (sb!impl::constant-form-value restp))
                (with-unique-names (emf-n)
                  `(locally
                       (declare (optimize (sb!c:insert-step-conditions 0)))
                     (let ((,emf-n ,emf))
                       (trace-emf-call ,emf-n ,restp (list ,@required-args ,@rest-arg))
                       (etypecase ,emf-n
                         (fast-method-call
                          ,(if more-arg
                               `(invoke-fast-method-call/more ,emf-n
                                                              ,@more-arg
                                                              ,@required-args)
                               `(invoke-fast-method-call ,emf-n
                                                         ,restp
                                                         ,@required-args
                                                         ,@rest-arg)))
                         ,@,(unless narrow
                              `(effective-method-optimized-slot-access-clause
                                emf-n restp required-args))
                         (method-call
                          (invoke-method-call ,emf-n ,restp ,@required-args
                                              ,@rest-arg))
                         (function
                          ,(if restp
                               `(apply ,emf-n ,@required-args ,@rest-arg)
                               `(funcall ,emf-n ,@required-args
                                         ,@rest-arg))))))))))
  (def invoke-effective-method-function nil)
  (def invoke-narrow-effective-method-function t))

(defun invoke-emf (emf args)
  (trace-emf-call emf t args)
  (etypecase emf
    (fast-method-call
     (let* ((arg-info (fast-method-call-arg-info emf))
            (restp (cdr arg-info))
            (nreq (car arg-info)))
       (if restp
           (apply (fast-method-call-function emf)
                  (fast-method-call-pv emf)
                  (fast-method-call-next-method-call emf)
                  args)
           (cond ((null args)
                  (if (eql nreq 0)
                      (invoke-fast-method-call emf nil)
                      (error 'simple-program-error
                             :format-control "invalid number of arguments: 0"
                             :format-arguments nil)))
                 ((null (cdr args))
                  (if (eql nreq 1)
                      (invoke-fast-method-call emf nil (car args))
                      (error 'simple-program-error
                             :format-control "invalid number of arguments: 1"
                             :format-arguments nil)))
                 ((null (cddr args))
                  (if (eql nreq 2)
                      (invoke-fast-method-call emf nil (car args) (cadr args))
                      (error 'simple-program-error
                             :format-control "invalid number of arguments: 2"
                             :format-arguments nil)))
                 (t
                  (apply (fast-method-call-function emf)
                         (fast-method-call-pv emf)
                         (fast-method-call-next-method-call emf)
                         args))))))
    (method-call
     (apply (method-call-function emf)
            args
            (method-call-call-method-args emf)))
    (fixnum
     (cond ((null args)
            (error 'simple-program-error
                   :format-control "invalid number of arguments: 0"
                   :format-arguments nil))
           ((null (cdr args))
            (let* ((slots (get-slots (car args)))
                   (value (clos-slots-ref slots emf)))
              (if (eq value +slot-unbound+)
                  (slot-unbound-internal (car args) emf)
                  value)))
           ((null (cddr args))
            (setf (clos-slots-ref (get-slots (cadr args)) emf)
                  (car args)))
           (t (error 'simple-program-error
                     :format-control "invalid number of arguments"
                     :format-arguments nil))))
    (fast-instance-boundp
     (if (or (null args) (cdr args))
         (error 'simple-program-error
                :format-control "invalid number of arguments"
                :format-arguments nil)
         (let ((slots (get-slots (car args))))
           (not (eq (clos-slots-ref slots (fast-instance-boundp-index emf))
                    +slot-unbound+)))))
    (function
     (apply emf args))))


(defmacro fast-call-next-method-body ((args next-method-call rest-arg)
                                      method-cell
                                      cnm-args)
  `(if ,next-method-call
       ,(let ((call `(invoke-narrow-effective-method-function
                      ,next-method-call
                      ,(not (null rest-arg))
                      :required-args ,args
                      :rest-arg ,(when rest-arg (list rest-arg)))))
             `(if ,cnm-args
                  (bind-args ((,@args
                               ,@(when rest-arg
                                       `(&rest ,rest-arg)))
                              ,cnm-args)
                    ,call)
                  ,call))
       (call-no-next-method ',method-cell
                            ,@args
                            ,@(when rest-arg
                                    `(,rest-arg)))))

(defmacro bind-fast-lexical-method-functions
    ((args rest-arg next-method-call (&key
                                      call-next-method-p
                                      setq-p
                                      parameters-setqd
                                      method-cell
                                      applyp))
     &body body
     &environment env)
  (let* ((next-method-p-def
          `((next-method-p ()
              (declare (optimize (sb!c:insert-step-conditions 0)))
              (not (null ,next-method-call)))))
         (rebindings (when (or setq-p call-next-method-p)
                       (mapcar (lambda (x) (list x x)) parameters-setqd))))
    (if (not (or call-next-method-p setq-p applyp))
        `(flet ,next-method-p-def
           (declare (ignorable #'next-method-p))
           ,@body)
        `(flet (,@(when call-next-method-p
                    `((call-next-method (&rest cnm-args)
                        (declare (dynamic-extent cnm-args)
                                 (muffle-conditions code-deletion-note)
                                 (optimize (sb!c:insert-step-conditions 0)))
                        ,@(if (safe-code-p env)
                              `((%check-cnm-args cnm-args (list ,@args)
                                                 ',method-cell))
                              nil)
                        (fast-call-next-method-body (,args
                                                     ,next-method-call
                                                     ,rest-arg)
                            ,method-cell
                            cnm-args))))
                ,@next-method-p-def)
           (declare (ignorable #'next-method-p))
           (let ,rebindings
             ,@body)))))


;;; CMUCL comment (Gerd Moellmann):
;;;
;;; The standard says it's an error if CALL-NEXT-METHOD is called with
;;; arguments, and the set of methods applicable to those arguments is
;;; different from the set of methods applicable to the original
;;; method arguments.  (According to Barry Margolin, this rule was
;;; probably added to ensure that before and around methods are always
;;; run before primary methods.)
;;;
;;; This could be optimized for the case that the generic function
;;; doesn't have hairy methods, does have standard method combination,
;;; is a standard generic function, there are no methods defined on it
;;; for COMPUTE-APPLICABLE-METHODS and probably a lot more of such
;;; preconditions.  That looks hairy and is probably not worth it,
;;; because this check will never be fast.
(defun %check-cnm-args (cnm-args orig-args method-cell)
  ;; 1. Check for no arguments.
  (when cnm-args
    (let* ((gf (method-generic-function (car method-cell)))
           (nreq (generic-function-nreq gf)))
      (declare (fixnum nreq))
      ;; 2. Requirement arguments pairwise: if all are EQL, the applicable
      ;; methods must be the same. This takes care of the relatively common
      ;; case of twiddling with &KEY arguments without being horribly
      ;; expensive.
      (unless (do ((orig orig-args (cdr orig))
                   (args cnm-args (cdr args))
                   (n nreq (1- nreq)))
                  ((zerop n) t)
                (unless (and orig args (eql (car orig) (car args)))
                  (return nil)))
        ;; 3. Only then do the full check.
        (let ((omethods (compute-applicable-methods gf orig-args))
              (nmethods (compute-applicable-methods gf cnm-args)))
          (unless (equal omethods nmethods)
            (error "~@<The set of methods ~S applicable to argument~P ~
                    ~{~S~^, ~} to call-next-method is different from ~
                    the set of methods ~S applicable to the original ~
                    method argument~P ~{~S~^, ~}.~@:>"
                   nmethods (length cnm-args) cnm-args omethods
                   (length orig-args) orig-args)))))))

;; FIXME: replacing this entire mess with DESTRUCTURING-BIND would correct
;; problems similar to those already solved by a correct implementation
;; of DESTRUCTURING-BIND, such as incorrect binding order:
;; e.g. (macroexpand-1 '(bind-args ((&optional (x nil xsp)) args) (form)))
;;      -> (LET* ((.ARGS-TAIL. ARGS) (XSP (NOT (NULL .ARGS-TAIL.))) (X ...)))
;; It's mostly irrelevant unless a method uses CALL-NEXT-METHOD though.
(defmacro bind-args ((lambda-list args) &body body)
  (let ((args-tail '.args-tail.)
        (key '.key.)
        (state 'required))
    (flet ((process-var (var)
             (if (memq var lambda-list-keywords)
                 (progn
                   (case var
                     (&optional       (setq state 'optional))
                     (&key            (setq state 'key))
                     (&allow-other-keys)
                     (&rest           (setq state 'rest))
                     (&aux            (setq state 'aux))
                     (otherwise
                      (error
                       "encountered the non-standard lambda list keyword ~S"
                       var)))
                   nil)
                 (case state
                   (required `((,var (pop ,args-tail))))
                   (optional (cond ((not (consp var))
                                    `((,var (when ,args-tail
                                              (pop ,args-tail)))))
                                   ((null (cddr var))
                                    `((,(car var) (if ,args-tail
                                                      (pop ,args-tail)
                                                      ,(cadr var)))))
                                   (t
                                    `((,(caddr var) (not (null ,args-tail)))
                                      (,(car var) (if ,args-tail
                                                      (pop ,args-tail)
                                                      ,(cadr var)))))))
                   (rest `((,var ,args-tail)))
                   (key (cond ((not (consp var))
                               `((,var (car
                                        (get-key-arg-tail ,(keywordicate var)
                                                          ,args-tail)))))
                              ((null (cddr var))
                               (multiple-value-bind (keyword variable)
                                   (if (consp (car var))
                                       (values (caar var)
                                               (cadar var))
                                       (values (keywordicate (car var))
                                               (car var)))
                                 `((,key (get-key-arg-tail ',keyword
                                                           ,args-tail))
                                   (,variable (if ,key
                                                  (car ,key)
                                                  ,(cadr var))))))
                              (t
                               (multiple-value-bind (keyword variable)
                                   (if (consp (car var))
                                       (values (caar var)
                                               (cadar var))
                                       (values (keywordicate (car var))
                                               (car var)))
                                 `((,key (get-key-arg-tail ',keyword
                                                           ,args-tail))
                                   (,(caddr var) (not (null,key)))
                                   (,variable (if ,key
                                                  (car ,key)
                                                  ,(cadr var))))))))
                   (aux `(,var))))))
      (let ((bindings (mapcan #'process-var lambda-list)))
        `(let* ((,args-tail ,args)
                ,@bindings
                (.dummy0.
                 ,@(when (eq state 'optional)
                     `((unless (null ,args-tail)
                         (error 'simple-program-error
                                :format-control "surplus arguments: ~S"
                                :format-arguments (list ,args-tail)))))))
           (declare (ignorable ,args-tail .dummy0.))
           ,@body)))))

(defun get-key-arg-tail (keyword list)
  (loop for (key . tail) on list by #'cddr
        when (null tail) do
          ;; FIXME: Do we want to export this symbol? Or maybe use an
          ;; (ERROR 'SIMPLE-PROGRAM-ERROR) form?
          (sb!c::%odd-key-args-error)
        when (eq key keyword)
          return tail))

(defun walk-method-lambda (method-lambda required-parameters env slots)
  (let (;; flag indicating that CALL-NEXT-METHOD should be in the
        ;; method definition
        (call-next-method-p nil)
        ;; a list of all required parameters whose bindings might be
        ;; modified in the method body.
        (parameters-setqd nil))
    (flet ((walk-function (form context env)
             (unless (and (eq context :eval) (consp form))
               (return-from walk-function form))
             (case (car form)
               (call-next-method
                    ;; hierarchy: nil -> :simple -> T.
                    (unless (eq call-next-method-p t)
                      (setq call-next-method-p (if (cdr form) t :simple)))
                    form)
               ((setq multiple-value-setq)
                    ;; The walker will split (SETQ A 1 B 2) to
                    ;; separate (SETQ A 1) and (SETQ B 2) forms, so we
                    ;; only need to handle the simple case of SETQ
                    ;; here.
                    (let ((vars (if (eq (car form) 'setq)
                                    (list (second form))
                                    (second form))))
                      (dolist (var vars)
                        ;; Note that we don't need to check for
                        ;; %VARIABLE-REBINDING declarations like is
                        ;; done in CAN-OPTIMIZE-ACCESS1, since the
                        ;; bindings that will have that declation will
                        ;; never be SETQd.
                        (when (var-declaration '%class var env)
                          ;; If a parameter binding is shadowed by
                          ;; another binding it won't have a %CLASS
                          ;; declaration anymore, and this won't get
                          ;; executed.
                          (pushnew var parameters-setqd :test #'eq))))
                    form)
               (function
                (when (equal (cdr form) '(call-next-method))
                  (setq call-next-method-p t))
                form)
               ((slot-value set-slot-value slot-boundp)
                (if (sb-xc:constantp (third form) env)
                    (let ((fun (ecase (car form)
                                 (slot-value #'optimize-slot-value)
                                 (set-slot-value #'optimize-set-slot-value)
                                 (slot-boundp #'optimize-slot-boundp))))
                      (funcall fun form slots required-parameters env))
                    form))
               (t form))))

      (let ((walked-lambda (walk-form method-lambda env #'walk-function)))
        ;;; FIXME: the walker's rewriting of the source code causes
        ;;; trouble when doing code coverage. The rewrites should be
        ;;; removed, and the same operations done using
        ;;; compiler-macros or tranforms.
        (values (if (sb!c:policy env (= sb!c:store-coverage-data 0))
                    walked-lambda
                    method-lambda)
                call-next-method-p
                (not (null parameters-setqd))
                parameters-setqd)))))

(defun generic-function-name-p (name)
  (and (legal-fun-name-p name)
       (fboundp name)
       (if (eq **boot-state** 'complete)
           (standard-generic-function-p (gdefinition name))
           (funcallable-instance-p (gdefinition name)))))

(defun method-plist-value (method key &optional default)
  (let ((plist (if (consp method)
                   (getf (early-method-initargs method) 'plist)
                   (object-plist method))))
    (getf plist key default)))

(defun (setf method-plist-value) (new-value method key &optional default)
  (if (consp method)
      (setf (getf (getf (early-method-initargs method) 'plist) key default)
            new-value)
      (setf (getf (object-plist method) key default) new-value)))

(defun load-defmethod (class name quals specls ll initargs source-location)
  (let ((method-cell (getf initargs 'method-cell)))
    (setq initargs (copy-tree initargs))
    (when method-cell
      (setf (getf initargs 'method-cell) method-cell))
    #+nil
    (setf (getf (getf initargs 'plist) :name)
          (make-method-spec name quals specls))
    (load-defmethod-internal class name quals specls
                             ll initargs source-location)))

(defun load-defmethod-internal
    (method-class gf-spec qualifiers specializers lambda-list
                  initargs source-location)
  (when (and (eq **boot-state** 'complete)
             (fboundp gf-spec))
    (let* ((gf (fdefinition gf-spec))
           (method (and (generic-function-p gf)
                        (generic-function-methods gf)
                        (find-method gf qualifiers specializers nil))))
      (when method
        (warn 'sb!kernel:redefinition-with-defmethod
              :name gf-spec
              :new-location source-location
              :old-method method
              :qualifiers qualifiers :specializers specializers))))
  (let ((method (apply #'add-named-method
                       gf-spec qualifiers specializers lambda-list
                       :definition-source source-location
                       initargs)))
    (unless (or (eq method-class 'standard-method)
                (eq (find-class method-class nil) (class-of method)))
      ;; FIXME: should be STYLE-WARNING?
      (format *error-output*
              "~&At the time the method with qualifiers ~:S and~%~
               specializers ~:S on the generic function ~S~%~
               was compiled, the method-class for that generic function was~%~
               ~S. But, the method class is now ~S, this~%~
               may mean that this method was compiled improperly.~%"
              qualifiers specializers gf-spec
              method-class (class-name (class-of method))))
    method))

(defun make-method-spec (gf qualifiers specializers)
  (let ((name (generic-function-name gf))
        (unparsed-specializers (unparse-specializers gf specializers)))
    `(slow-method ,name ,@qualifiers ,unparsed-specializers)))

(defun initialize-method-function (initargs method)
  (let* ((mf (getf initargs :function))
         (mff (and (typep mf '%method-function)
                   (%method-function-fast-function mf)))
         (plist (getf initargs 'plist))
         (name (getf plist :name))
         (method-cell (getf initargs 'method-cell)))
    (when method-cell
      (setf (car method-cell) method))
    (when name
      (when mf
        (setq mf (set-fun-name mf name)))
      (when (and mff (consp name) (eq (car name) 'slow-method))
        (let ((fast-name `(fast-method ,@(cdr name))))
          (set-fun-name mff fast-name))))
    (when plist
      (let ((plist plist))
        (let ((snl (getf plist :slot-name-lists)))
          (when snl
            (setf (method-plist-value method :pv-table)
                  (intern-pv-table :slot-name-lists snl))))))))

(defun analyze-lambda-list (lambda-list)
  (multiple-value-bind (llks required optional rest keywords)
      ;; We say "&MUMBLE is not allowed in a generic function lambda list"
      ;; whether this is called by DEFMETHOD or DEFGENERIC.
      ;; [It is used for either. Why else recognize and silently ignore &AUX?]
      (parse-lambda-list lambda-list
                         :accept (lambda-list-keyword-mask
                                    '(&optional &rest &key &allow-other-keys &aux))
                         :silent t
                         :context "a generic function lambda list")
    (declare (ignore rest))
    (values llks (length required) (length optional)
            (mapcar #'parse-key-arg-spec keywords) keywords)))

;; FIXME: this does more than return an FTYPE from a lambda list -
;; it unions the type with an existing ctype object. It needs a better name,
;; and to be reimplemented as "union and call sb!c::ftype-from-lambda-list".
(defun ftype-declaration-from-lambda-list (lambda-list name)
  (multiple-value-bind (llks nrequired noptional keywords keyword-parameters)
      (analyze-lambda-list lambda-list)
    (declare (ignore keyword-parameters))
    (let* ((old (proclaimed-ftype name)) ;FIXME:FDOCUMENTATION instead?
           (old-ftype (if (fun-type-p old) old nil))
           (old-restp (and old-ftype (fun-type-rest old-ftype)))
           (old-keys (and old-ftype
                          (mapcar #'key-info-name
                                  (fun-type-keywords
                                   old-ftype))))
           (old-keysp (and old-ftype (fun-type-keyp old-ftype)))
           (old-allowp (and old-ftype
                            (fun-type-allowp old-ftype)))
           (keywords (union old-keys (mapcar #'parse-key-arg-spec keywords))))
      `(function ,(append (make-list nrequired :initial-element t)
                          (when (plusp noptional)
                            (append '(&optional)
                                    (make-list noptional :initial-element t)))
                          (when (or (ll-kwds-restp llks) old-restp)
                            '(&rest t))
                          (when (or (ll-kwds-keyp llks) old-keysp)
                            (append '(&key)
                                    (mapcar (lambda (key)
                                              `(,key t))
                                            keywords)
                                    (when (or (ll-kwds-allowp llks) old-allowp)
                                      '(&allow-other-keys)))))
                 *))))



(defconstant +sm-specializers-index+
  (!bootstrap-slot-index 'standard-method 'specializers))
(defconstant +sm-%function-index+
  (!bootstrap-slot-index 'standard-method '%function))
(defconstant +sm-qualifiers-index+
  (!bootstrap-slot-index 'standard-method 'qualifiers))

;;; FIXME: we don't actually need this; we could test for the exact
;;; class and deal with it as appropriate.  In fact we probably don't
;;; need it anyway because we only use this for METHOD-SPECIALIZERS on
;;; the standard reader method for METHOD-SPECIALIZERS.  Probably.
(dolist (s '(specializers %function))
  (aver (= (symbol-value (intern (format nil "+SM-~A-INDEX+" s)))
           (!bootstrap-slot-index 'standard-reader-method s)
           (!bootstrap-slot-index 'standard-writer-method s)
           (!bootstrap-slot-index 'standard-boundp-method s)
           (!bootstrap-slot-index 'global-reader-method s)
           (!bootstrap-slot-index 'global-writer-method s)
           (!bootstrap-slot-index 'global-boundp-method s))))

(defvar *standard-method-class-names*
  '(standard-method standard-reader-method
    standard-writer-method standard-boundp-method
    global-reader-method global-writer-method
    global-boundp-method))

(declaim (list **standard-method-classes**))
(defglobal **standard-method-classes** nil)

(defun safe-method-specializers (method)
  (if (member (class-of method) **standard-method-classes** :test #'eq)
      (clos-slots-ref (std-instance-slots method) +sm-specializers-index+)
      (method-specializers method)))
(defun safe-method-fast-function (method)
  (let ((mf (safe-method-function method)))
    (and (typep mf '%method-function)
         (%method-function-fast-function mf))))
(defun safe-method-function (method)
  (if (member (class-of method) **standard-method-classes** :test #'eq)
      (clos-slots-ref (std-instance-slots method) +sm-%function-index+)
      (method-function method)))
(defun safe-method-qualifiers (method)
  (if (member (class-of method) **standard-method-classes** :test #'eq)
      (clos-slots-ref (std-instance-slots method) +sm-qualifiers-index+)
      (method-qualifiers method)))

(defun set-arg-info1 (gf arg-info new-method methods was-valid-p first-p)
  (let* ((existing-p (and methods (cdr methods) new-method))
         (nreq (length (arg-info-metatypes arg-info)))
         (metatypes (if existing-p
                        (arg-info-metatypes arg-info)
                        (make-list nreq)))
         (type (if existing-p
                   (gf-info-simple-accessor-type arg-info)
                   nil)))
    (when (arg-info-valid-p arg-info)
      (dolist (method (if new-method (list new-method) methods))
        (let* ((specializers (if (or (eq **boot-state** 'complete)
                                     (not (consp method)))
                                 (safe-method-specializers method)
                                 (early-method-specializers method t)))
               (class (if (or (eq **boot-state** 'complete) (not (consp method)))
                          (class-of method)
                          (early-method-class method)))
               (new-type
                (when (and class
                           (or (not (eq **boot-state** 'complete))
                               (eq (generic-function-method-combination gf)
                                   *standard-method-combination*)))
                  (cond ((or (eq class *the-class-standard-reader-method*)
                             (eq class *the-class-global-reader-method*))
                         'reader)
                        ((or (eq class *the-class-standard-writer-method*)
                             (eq class *the-class-global-writer-method*))
                         'writer)
                        ((or (eq class *the-class-standard-boundp-method*)
                             (eq class *the-class-global-boundp-method*))
                         'boundp)))))
          (setq metatypes (mapcar #'raise-metatype metatypes specializers))
          (setq type (cond ((null type) new-type)
                           ((eq type new-type) type)
                           (t nil)))))
      (setf (arg-info-metatypes arg-info) metatypes)
      (setf (gf-info-simple-accessor-type arg-info) type)))
  (when (or (not was-valid-p) first-p)
    (multiple-value-bind (c-a-m-emf std-p)
        (if (early-gf-p gf)
            (values t t)
            (compute-applicable-methods-emf gf))
      (setf (gf-info-static-c-a-m-emf arg-info) c-a-m-emf)
      (setf (gf-info-c-a-m-emf-std-p arg-info) std-p)
      (unless (gf-info-c-a-m-emf-std-p arg-info)
        (setf (gf-info-simple-accessor-type arg-info) t))))
  (unless was-valid-p
    (let ((name (if (eq **boot-state** 'complete)
                    (generic-function-name gf)
                    (!early-gf-name gf))))
      (setf (gf-precompute-dfun-and-emf-p arg-info)
            (cond
              ((and (consp name)
                    (member (car name)
                            *internal-pcl-generalized-fun-name-symbols*))
                nil)
              (t (let* ((symbol (fun-name-block-name name))
                        (package (symbol-package symbol)))
                   (and (or (eq package *pcl-package*)
                            (memq package (package-use-list *pcl-package*)))
                        (not (eq package *cl-package*))
                        ;; FIXME: this test will eventually be
                        ;; superseded by the *internal-pcl...* test,
                        ;; above.  While we are in a process of
                        ;; transition, however, it should probably
                        ;; remain.
                        (not (find #\Space (symbol-name symbol))))))))))
  (setf (gf-info-fast-mf-p arg-info)
        (or (not (eq **boot-state** 'complete))
            (let* ((method-class (generic-function-method-class gf))
                   (methods (compute-applicable-methods
                             #'make-method-lambda
                             (list gf (class-prototype method-class)
                                   '(lambda) nil))))
              (and methods (null (cdr methods))
                   (let ((specls (method-specializers (car methods))))
                     (and (classp (car specls))
                          (eq 'standard-generic-function
                              (class-name (car specls)))
                          (classp (cadr specls))
                          (eq 'standard-method
                              (class-name (cadr specls)))))))))
  arg-info)


;;; Makes a method early on in the bootstrapping process, and returns
;;; information about that method.
;;; CLASS is the name of the class
;;; QUALIFIERS are the method qualifiers for the method
;;; SPECIALIZERS is a list of either class names (symbols) or class
;;; objects (anything that is not a symbol is assumed to be a class object).
;;; Returns RESULT, a list of the form
;;;   (:early-method
;;;    <the :function initarg value>
;;;    <the %method-function-fast-function or NIL>
;;;    <the parsed specializers (objects not symbols)>
;;;    REAL-MAKE-METHOD-ARGS)
;;; Where REAL-MAKE-METHOD-ARGS is a list to which REAL-MAKE-A-METHOD can
;;; be applied make a real method corresponding to this early one.
;;; Note that INITIALIZE-METHOD-FUNCTION is called with INITARGS and
;;; RESULT after RESULT is computed and before returning RESULT.
(defun !early-make-a-method (class qualifiers arglist specializers initargs doc
                            &key slot-name object-class method-class-function
                            definition-source)
  (aver (notany #'sb!pcl::eql-specializer-p specializers))
  (binding*
      ;; Figure out whether we got class objects or class names as the
      ;; specializers and set parsed and unparsed appropriately. If we
      ;; got class objects, then we can compute unparsed, but if we
      ;; got class names we don't try to compute parsed.
      (((parsed unparsed)
        (if (every #'classp specializers)
            (values specializers
                    (mapcar (lambda (s)
                              (if (eq s t) t (class-name s)))
                            specializers))
            (values () specializers)))
       (result
        (list :early-method

              (getf initargs :function)
              (let ((mf (getf initargs :function)))
                (aver mf)
                (and (typep mf '%method-function)
                     (%method-function-fast-function mf)))

              ;; the parsed specializers. This is used by
              ;; EARLY-METHOD-SPECIALIZERS to cache the parse.
              ;; Note that this only comes into play when there is
              ;; more than one early method on an early gf.
              parsed

              ;; A list to which REAL-MAKE-A-METHOD can be applied
              ;; to make a real method corresponding to this early
              ;; one.
              (append
               (list class qualifiers arglist unparsed
                     initargs doc)
               (when slot-name
                 (list :slot-name slot-name :object-class object-class
                       :method-class-function method-class-function))
               (list :definition-source definition-source)))))
    (initialize-method-function initargs result)
    result))

(defun real-make-a-method
       (class qualifiers lambda-list specializers initargs doc
        &rest args &key slot-name object-class method-class-function
        definition-source)
  (if method-class-function
      (let* ((object-class (if (classp object-class) object-class
                               (find-class object-class)))
             (slots (class-direct-slots object-class))
             (slot-definition (find slot-name slots
                                    :key #'slot-definition-name)))
        (aver slot-name)
        (aver slot-definition)
        (let ((initargs (list* :qualifiers qualifiers :lambda-list lambda-list
                               :specializers specializers :documentation doc
                               :slot-definition slot-definition
                               :slot-name slot-name initargs)))
          (apply #'make-instance
                 (apply method-class-function object-class slot-definition
                        initargs)
                 :definition-source definition-source
                 initargs)))
      (apply #'make-instance class :qualifiers qualifiers
             :lambda-list lambda-list :specializers specializers
             :documentation doc (append args initargs))))

(defun early-method-function (early-method)
  (values (cadr early-method) (caddr early-method)))

(defun early-method-class (early-method)
  (find-class (car (fifth early-method))))

(defun early-method-standard-accessor-p (early-method)
  (let ((class (first (fifth early-method))))
    (or (eq class 'standard-reader-method)
        (eq class 'standard-writer-method)
        (eq class 'standard-boundp-method))))

(defun early-method-standard-accessor-slot-name (early-method)
  (eighth (fifth early-method)))

;;; Fetch the specializers of an early method. This is basically just
;;; a simple accessor except that when the second argument is t, this
;;; converts the specializers from symbols into class objects. The
;;; class objects are cached in the early method, this makes
;;; bootstrapping faster because the class objects only have to be
;;; computed once.
;;;
;;; NOTE:
;;;  The second argument should only be passed as T by
;;;  early-lookup-method. This is to implement the rule that only when
;;;  there is more than one early method on a generic function is the
;;;  conversion from class names to class objects done. This
;;;  corresponds to the fact that we are only allowed to have one
;;;  method on any generic function up until the time classes exist.
(defun early-method-specializers (early-method &optional objectsp)
  (if (and (listp early-method)
           (eq (car early-method) :early-method))
      (cond ((eq objectsp t)
             (or (fourth early-method)
                 (setf (fourth early-method)
                       (mapcar #'find-class (cadddr (fifth early-method))))))
            (t
             (fourth (fifth early-method))))
      (error "~S is not an early-method." early-method)))

(defun early-method-qualifiers (early-method)
  (second (fifth early-method)))

(defun early-method-lambda-list (early-method)
  (third (fifth early-method)))

(defun early-method-initargs (early-method)
  (fifth (fifth early-method)))

(defun (setf early-method-initargs) (new-value early-method)
  (setf (fifth (fifth early-method)) new-value))

(defun !early-add-named-method (generic-function-name qualifiers
                               specializers arglist &rest initargs
                               &key documentation definition-source
                               &allow-other-keys)
  (let* (;; we don't need to deal with the :generic-function-class
         ;; argument here because the default,
         ;; STANDARD-GENERIC-FUNCTION, is right for all early generic
         ;; functions.  (See REAL-ADD-NAMED-METHOD)
         (gf (ensure-generic-function generic-function-name))
         (existing
           (dolist (m (early-gf-methods gf))
             (when (and (equal (early-method-specializers m) specializers)
                        (equal (early-method-qualifiers m) qualifiers))
               (return m)))))
    (setf (getf (getf initargs 'plist) :name)
          (make-method-spec gf qualifiers specializers))
    (let ((new (make-a-method 'standard-method qualifiers arglist
                              specializers initargs documentation
                              :definition-source definition-source)))
      (when existing (remove-method gf existing))
      (add-method gf new))))

;;; This is the early version of ADD-METHOD. Later this will become a
;;; generic function. See !FIX-EARLY-GENERIC-FUNCTIONS which has
;;; special knowledge about ADD-METHOD.
#+sb-xc
(defun add-method (generic-function method)
  (when (not (fsc-instance-p generic-function))
    (error "Early ADD-METHOD didn't get a funcallable instance."))
  (when (not (and (listp method) (eq (car method) :early-method)))
    (error "Early ADD-METHOD didn't get an early method."))
  (push method (early-gf-methods generic-function))
  (set-arg-info generic-function :new-method method)
  (unless (assoc (!early-gf-name generic-function)
                 *!generic-function-fixups*
                 :test #'equal)
    (update-dfun generic-function)))

;;; This is the early version of REMOVE-METHOD. See comments on
;;; the early version of ADD-METHOD.
#+sb-xc
(defun remove-method (generic-function method)
  (when (not (fsc-instance-p generic-function))
    (error "An early remove-method didn't get a funcallable instance."))
  (when (not (and (listp method) (eq (car method) :early-method)))
    (error "An early remove-method didn't get an early method."))
  (setf (early-gf-methods generic-function)
        (remove method (early-gf-methods generic-function)))
  (set-arg-info generic-function)
  (unless (assoc (!early-gf-name generic-function)
                 *!generic-function-fixups*
                 :test #'equal)
    (update-dfun generic-function)))

;;; This is the early version of GET-METHOD. See comments on the early
;;; version of ADD-METHOD.
(defun get-method (generic-function qualifiers specializers
                                    &optional (errorp t))
  (if (early-gf-p generic-function)
      (or (dolist (m (early-gf-methods generic-function))
            (when (and (or (equal (early-method-specializers m nil)
                                  specializers)
                           (equal (early-method-specializers m t)
                                  specializers))
                       (equal (early-method-qualifiers m) qualifiers))
              (return m)))
          (if errorp
              (error "can't get early method")
              nil))
      (real-get-method generic-function qualifiers specializers errorp)))

(setq **boot-state** 'early)
