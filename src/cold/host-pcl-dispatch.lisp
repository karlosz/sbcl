;;;; Host PCL's generic functions dispatched by the host's CLOS
;;;;
;;;; PCL's objects on the host are proxies (host-pcl.lisp): structures that
;;;; the host's CLOS cannot tell apart by their PCL classes.  So each PCL
;;;; class C has an empty host class D(C), its dispatch class, with the
;;;; dispatch classes of C's direct superclasses as its own, and each object
;;;; is represented in a call by a key: the prototype of D(its PCL class).
;;;; A PCL generic function F has a host generic function on keys, whose
;;;; lambda list is (K1 ... Kn &REST ARGS) for F's n required arguments; its
;;;; methods are PCL's DEFMETHODs and DEFCLASS accessors, translated from the
;;;; source, which run their bodies on the real arguments ARGS.
;;;;
;;;; So PCL's generic functions work on the host without PCL's own dispatch,
;;;; which is what PCL's bootstrap builds.

(in-package "SB-PCL")

;;; make-host-2's and genesis's records, and host PCL's generic functions,
;;; which it makes as it builds
(declaim (special sb-cold::*host-pcl-record*)
         (ftype function add-reader-method add-writer-method class-wrapper
                create-gf-lambda-list parse-specialized-lambda-list
                generic-function-initial-methods (setf generic-function-initial-methods)
                method-qualifiers method-specializers))

(unless (find-package "SB-PCL-DISPATCH")
  (make-package "SB-PCL-DISPATCH" :use nil))

;;; The host symbol naming the dispatch class or generic function for NAME
(defun dispatch-symbol (name)
  (intern (let ((*package* (find-package "KEYWORD"))) (prin1-to-string name))
          "SB-PCL-DISPATCH"))

;;;; Dispatch classes

;;; PCL class name -> direct superclass names, from DEFCLASS forms
(defvar *dispatch-supers* (make-hash-table :test 'eq))
(defvar *dispatch-classes* (make-hash-table :test 'eq)) ; name -> (class . prototype)

(defun dispatch-class-supers (name)
  (multiple-value-bind (supers found) (gethash name *dispatch-supers*)
    (if found
        (or supers (if (member name '(t)) nil '(standard-object)))
        ;; the cross-compiler's classoid: built-in, structure, condition
        (let ((classoid (sb-kernel:find-classoid name nil)))
          (if classoid
              (mapcar (lambda (super) (sb-kernel:classoid-name super))
                      (sb-kernel:classoid-direct-superclasses classoid))
              (error "host PCL: no class ~S to dispatch on" name))))))

(defun dispatch-class (name)
  (car (dispatch-class-entry name)))
(defun dispatch-class-entry (name)
  (or (gethash name *dispatch-classes*)
      (let* ((supers (unless (eq name t)
                       (or (mapcar #'dispatch-class (dispatch-class-supers name))
                           (list (dispatch-class t)))))
             (symbol (dispatch-symbol name))
             (class (eval `(cl:defclass ,symbol ,(mapcar #'cl:class-name supers) ()))))
        (setf (gethash name *dispatch-classes*) (cons class (cl:make-instance class))))))

;;; The PCL class name of X, as host PCL sees it
(defun dispatch-class-name-of (x)
  ;; T for the host's own objects, such as its lexical environments
  (let ((layout (if (and (typep x 'structure-object) (not (host-%instancep x)))
                    nil
                    (sb-kernel:layout-of x))))
    (cond ((null layout) t)
          ((host-wrapper-p layout) (sb-kernel::layout-classoid-name layout))
          (t (sb-kernel:classoid-name (sb-kernel:layout-classoid layout))))))

(defun dispatch-key (x)
  (cdr (dispatch-class-entry (dispatch-class-name-of x))))

;;;; Dispatch generic functions

;;; PCL name -> (number of keys DEFGENERIC-options lambda-list), noted as the
;;; source is read; the host generic functions are made by BUILD-DISPATCH
(defvar *dispatch-gfs* (make-hash-table :test 'equal))

(defun dispatch-required-count (lambda-list)
  (or (position-if (lambda (x) (member x lambda-list-keywords)) lambda-list)
      (length lambda-list)))

(defun note-dispatch-gf (name nreq &optional options lambda-list)
  (let ((old (gethash name *dispatch-gfs*)))
    (cond ((null old) (setf (gethash name *dispatch-gfs*) (list nreq options lambda-list)))
          ((/= (car old) nreq)
           (error "host PCL: ~S has ~D required arguments, not ~D" name (car old) nreq))
          (t (when options (setf (second old) options))
             (when lambda-list (setf (third old) lambda-list))))))
(defun make-dispatch-gf (name entry)
  (destructuring-bind (nreq options lambda-list) entry
    (declare (ignore lambda-list))
    (eval `(cl:defgeneric ,(dispatch-symbol name)
               (,@(loop for i below nreq collect (dispatch-symbol (format nil "K~D" i)))
                &rest args)
             ,@options))))

;;; Call the PCL generic function NAME on ARGS through the host's dispatch
(defun dispatch-call (name &rest args)
  (let ((nreq (car (or (gethash name *dispatch-gfs*)
                       (error "host PCL: no dispatch for ~S" name)))))
    (apply (fdefinition (dispatch-symbol name))
           (nconc (loop for arg in args repeat nreq collect (dispatch-key arg)) args))))

;;;; Translating PCL's definitions

;;; BODY's forms and its declarations (and documentation)
(defun split-body (body)
  (let ((forms body) (declarations nil))
    (loop while (and forms (or (typep (car forms) '(cons (eql declare)))
                               (and (stringp (car forms)) (cdr forms))))
          do (push (pop forms) declarations))
    (values forms (nreverse declarations))))

;;; A host method of the dispatch generic function of NAME: PCL's
;;; QUALIFIERS, specialized LAMBDA-LIST and BODY
(defun dispatch-method-form (name qualifiers lambda-list body)
  (let* ((nreq (dispatch-required-count lambda-list))
         (required (subseq lambda-list 0 nreq))
         (vars (mapcar (lambda (p) (if (consp p) (car p) p)) required))
         (keys (loop for i below nreq collect (dispatch-symbol (format nil "K~D" i))))
         (args (dispatch-symbol "ARGS"))
         (specializers (mapcar (lambda (p) (if (consp p) (second p) t)) required)))
    (note-dispatch-gf name nreq)
    ;; a generic function without a DEFGENERIC has its first method's lambda
    ;; list, as ENSURE-GENERIC-FUNCTION gives it (calls of it cross-compiled
    ;; before the method is made need it)
    (let ((entry (gethash name *dispatch-gfs*)))
      (unless (third entry)
        (setf (third entry)
              (create-gf-lambda-list (nth-value 1 (parse-specialized-lambda-list lambda-list))))))
    ;; An EQL specializer (defcombin.lisp's on *STANDARD-METHOD-COMBINATION*):
    ;; the host's dispatch is on classes, and its bootstrap needs no such
    ;; method; the method itself is PCL's, made as any other
    (when (some #'consp specializers)
      (return-from dispatch-method-form '(progn)))
    `(progn
       (mapc #'dispatch-class ',specializers)
       (cl:defmethod ,(dispatch-symbol name) ,@qualifiers
           (,@(mapcar (lambda (key specializer) `(,key ,(dispatch-symbol specializer)))
                      keys specializers)
            &rest ,args)
         (declare (ignore ,@keys))
         ;; a method's keywords are checked by its generic function
         (apply (lambda (,@vars ,@(nthcdr nreq lambda-list)
                         ,@(and (member '&key lambda-list)
                                (not (member '&allow-other-keys lambda-list))
                                '(&allow-other-keys)))
                  (declare (ignorable ,@vars))
                  ,@(multiple-value-bind (forms declarations) (split-body body)
                      `(,@declarations
                        ;; as DEFMETHOD's, named by the generic function
                        (block ,(if (consp name) (second name) name) ,@forms))))
                ,args)))))

;;; DEFCLASS: its superclasses and its accessors' methods
(defun dispatch-defclass-forms (form)
  (destructuring-bind (name supers slots &rest options) (cdr form)
    (declare (ignore options))
    (setf (gethash name *dispatch-supers*) supers)
    (loop for slot in slots
          for slot-name = (if (consp slot) (car slot) slot)
          when (consp slot)
            nconc (loop for (key value) on (cdr slot) by #'cddr
                        when (member key '(:reader :accessor))
                          collect (dispatch-method-form
                                   value nil `((object ,name))
                                   `((slot-value object ',slot-name)))
                        when (member key '(:writer :accessor))
                          collect (let ((writer (if (eq key :accessor) `(setf ,value) value)))
                                    (dispatch-method-form
                                     writer nil `(new-value (object ,name))
                                     `((setf (slot-value object ',slot-name) new-value))))))))

;;; A DEFGENERIC as host PCL runs it: its :METHOD options' methods, its
;;; initial methods, are made of what the cross-compiler notes as any
;;; method's (MAKE-OPTION-METHODS)
(defun defgeneric-without-methods (form)
  `(,@(subseq form 0 3)
    ,@(remove-if (lambda (option) (typep option '(cons (eql :method)))) (nthcdr 3 form))))
;;; Those whose DEFGENERIC names their class (:GENERIC-FUNCTION-CLASS): their
;;; DEFGENERICs make them, once the class is made (MAKE-PCL-GENERIC-FUNCTIONS)
(defvar *dispatch-gfs-of-own-class* nil)

(defun dispatch-defgeneric-forms (form)
  (destructuring-bind (name lambda-list &rest options) (cdr form)
    (when (assoc :generic-function-class options)
      (pushnew name *dispatch-gfs-of-own-class* :test #'equal))
    (note-dispatch-gf name (dispatch-required-count lambda-list)
                      (remove-if-not (lambda (option) (eq (car option) :method-combination))
                                     options)
                      lambda-list)
    (loop for option in options
          when (eq (car option) :method)
            collect (let ((qualifiers (loop for x in (cdr option) until (listp x) collect x)))
                      (dispatch-method-form name qualifiers
                                            (nth (length qualifiers) (cdr option))
                                            (nthcdr (1+ (length qualifiers)) (cdr option)))))))

;;; The host forms for one of PCL's definitions, KIND as recorded
(defun dispatch-forms (kind form)
  (case kind
    (:class (dispatch-defclass-forms form))
    (:generic-function (dispatch-defgeneric-forms form))
    (:method
     (destructuring-bind (name &rest rest) (cdr form)
       (let ((qualifiers (loop for x in rest until (listp x) collect x)))
         (list (dispatch-method-form name qualifiers (nth (length qualifiers) rest)
                                     (nthcdr (1+ (length qualifiers)) rest))))))))

;;; The host's forms for PCL's DEFCLASSes, DEFGENERICs and DEFMETHODs, in order
(defvar *dispatch-forms* nil)
(defun note-dispatch-form (kind form)
  (let ((forms (dispatch-forms kind form)))
    (when forms
      (push (list* 'progn forms) *dispatch-forms*))))

;;; (once for PCL proper's, and again for the other files': their dispatch
;;; generic functions are made once, and the forms run once)
(defvar *dispatch-gfs-made* (make-hash-table :test 'equal))
(defun build-dispatch ()
  (let ((failures 0))
    (maphash (lambda (name entry)
               (unless (shiftf (gethash name *dispatch-gfs-made*) t)
                 (make-dispatch-gf name entry)))
             *dispatch-gfs*)
    (dolist (form (reverse (shiftf *dispatch-forms* nil)))
      (handler-case (eval form)
        (error (c)
          (incf failures)
          (format t "~&; host PCL dispatch: ~A~%" c))))
    (when (plusp failures)
      (format t "~&; host PCL dispatch: ~D failures~%" failures))))

;;; Proxies by their classes (they are circular)
(defun dispatch-printable (x &optional (depth 3))
  (cond ((or (host-instance-p x) (host-funcallable-record x))
         (list :a (dispatch-class-name-of x)))
        ((and (consp x) (plusp depth))
         (loop for y in x repeat 6 collect (dispatch-printable y (1- depth))))
        ((consp x) :more)
        (t x)))

;;;; PCL proper's definitions
;;;;
;;;; make-host-1 loads PCL proper's files, which defines its functions,
;;;; macros, variables and structures; the cross-compiler records its
;;;; classes, generic functions and methods and the toplevel forms marked to
;;;; wait (pre-warm.lisp).  Once PCL's generic functions dispatch on the host
;;;; (above), pass 2 runs those in order: DEFCLASS, DEFGENERIC and DEFMETHOD
;;;; by PCL's real protocol, whose generic functions all have their methods
;;;; already.  So PCL needs no early versions of its generic functions.

;;; (package . form), last first
(defvar *pass-2-forms* nil)
(defvar *pass-2-capturing-forms* nil)

;;; The forms that pass 2 runs, as loading PCL proper's files recorded them.
;;; A DEFMETHOD is the dispatch layer's only: the method itself is made of
;;; what the cross-compiler notes of it (MAKE-METHODS), as any other's.
(defun take-pass-2-forms ()
  (dolist (entry (reverse sb-cold::*host-pcl-record*))
    (destructuring-bind (kind package form) entry
      (note-dispatch-form kind form)
      (case kind
        (:capturing (push (cons package form) *pass-2-capturing-forms*))
        (:method)
        (:generic-function (push (cons package (defgeneric-without-methods form)) *pass-2-forms*))
        (t (push (cons package form) *pass-2-forms*))))))
;;; The methods of the files before boot.lisp are early, their generic
;;; functions being called in cold init before PCL's toplevel forms.
(defvar *methods-made* 0)               ; (MAKE-METHODS)
(defvar *methods-waiting* nil)          ; ... the records of, in order
(defvar *host-pcl-boot-seen* nil)
;;; Any file's DEFCLASS or DEFGENERIC but PCL proper's, as make-host-2
;;; compiles it (!XC-DEFINITION)
;;; ... whose class genesis knows as a type from where the file's fasl says
;;; so on, as the cross-compiler does (the target's constants of it):
;;; its object host PCL writes once it is built (src/cold/host-pcl-dump.lisp)
(defun note-class-for-genesis (name)
  (let ((file sb-c::*compile-object*))
    (when (typep file 'sb-c::fasl-output)
      (sb-c::dump-object :declare-classoid file)
      (sb-c::dump-object name file)
      (sb-c::dump-fop 'sb-fasl::fop-host-pcl-effect file 2))))
(defun host-pcl-define (kind form)
  (note-dispatch-form kind form)
  (when (eq kind :class)
    (push (second form) *compile-time-class-names*)
    (note-class-for-genesis (second form)))
  (push (cons *package* (if (eq kind :generic-function) (defgeneric-without-methods form) form))
        *pass-2-forms*)
  (call-as-host-pcl #'pass-2-definitions)
  (when (eq kind :generic-function)
    (make-option-methods form nil)
    (make-methods (shiftf *methods-waiting* nil))))

;;; Cross-compiled, DEFCLASS, DEFGENERIC, DEFMETHOD and
;;; DEFINE-METHOD-COMBINATION forms have their compile-time effects, and
;;; the rest of what they expand into is compiled into a function that is
;;; never called, which dumps the methods' functions for host PCL's methods
;;; (genesis finds them by their names).  A class has nothing more: host PCL
;;; makes it and its accessors' methods.
(defun compile-time-parts (expansion)
  (loop for x in (if (typep expansion '(cons (eql progn))) (cdr expansion) (list expansion))
        when (and (typep x '(cons (eql eval-when) (cons list)))
                  (member :compile-toplevel (second x)))
          collect `(eval-when (:compile-toplevel) ,@(cddr x))))
(defun host-pcl-compile-time-only (expansion)
  `(progn ,@(compile-time-parts expansion)
          (!host-form (lambda () (symbol-macrolet ((%in-host-form t)) ,expansion)))))
;;; A file's classes and generic functions are host PCL's as the
;;; cross-compiler compiles it, as the target's are as it loads the file
;;; (and its methods, of what the cross-compiler notes: MAKE-METHODS), but
;;; for PCL proper's, which host PCL has already (:HOST-DEFINITIONS)
(defvar *host-pcl-defining* nil)
;;; (target functions that macro bodies mention, which the host compiles)
(declaim (ftype function sb-mop:method-function class-wrapper))
(defun !xc-definition (kind form expansion)
  (if (and (member kind '(:class :generic-function)) *host-pcl-defining*)
      ;; (after the definition's compile-time effects for the cross-compiler)
      `(progn ,@(if (eq kind :class)
                    (compile-time-parts expansion)
                    (list (host-pcl-compile-time-only expansion)))
              (eval-when (:compile-toplevel) (host-pcl-define ',kind ',form)))
      (host-pcl-compile-time-only expansion)))

;;; The cross-compiler's DEFCLASS, DEFGENERIC, DEFMETHOD and
;;; DEFINE-METHOD-COMBINATION: what they expand into is !XC-DEFINITION's
;;; (its macroexpand hook, as their files may define them again as it
;;; compiles them).  A class that host PCL has already is nothing more to
;;; cross-compile: the cross-compiler knows it (its type, its accessors'
;;; types), as it knows a structure it has (SB-XC:DEFSTRUCT).
(defun xc-definition-macroexpand-hook (expander form env)
  (let ((kind (and (consp form) (!cross-compiling-p)
                   (case (car form)
                     (defclass :class)
                     (defgeneric :generic-function)
                     (defmethod :method)
                     (define-method-combination :method-combination)))))
    (cond ((not kind) (funcall expander form env))
          ;; (code of a definition already made, which is only compiled:
          ;; a DEFGENERIC's :METHODs)
          ((nth-value 1 (sb-xc:macroexpand-1 '%in-host-form env))
           (funcall expander form env))
          ;; Host PCL makes the definitions that the cross-compiler
          ;; processes as top level forms, as each build mode does: those
          ;; anywhere else would be the target's only, once cold init got
          ;; to them, not CLOS as cold init begins.
          ((not sb-kernel:*top-level-form-p*)
           (error "host PCL: ~S ~S is not a top level form" (car form) (second form)))
          ((and (eq kind :class) (!host-pcl-class-p (second form))) nil)
          (t (let ((expansion (funcall expander form env)))
               (case kind
                 (:method (setq expansion (expand-defmethod-once expansion env)))
                 ;; (another file's DEFGENERIC makes them once it has made
                 ;; the generic function: HOST-PCL-DEFINE)
                 (:generic-function
                  (unless *host-pcl-defining* (make-option-methods form env))))
               (!xc-definition kind form expansion))))))

;;; The methods that the DEFMETHOD being expanded notes, last first:
;;; (fun-name lambda-list source-location initargs), by the name of its
;;; function, of which host PCL makes the method (MAKE-METHODS).  INITARGS
;;; are its DEFMETHOD's (MAKE-METHOD-INITARGS-FORM-INTERNAL1).
(defvar *noted-methods*)
(defun note-method-info (fun-name lambda-list initargs)
  (push (list fun-name lambda-list (sb-c::make-definition-source-location) initargs)
        *noted-methods*))
;;; A top level DEFMETHOD's EXPANSION, its %DEFMETHOD-EXPANDERs expanded
;;; here, once, where they would be as top level forms, which notes the
;;; method: its expansion is compiled into a function instead
;;; (HOST-PCL-COMPILE-TIME-ONLY).  Host PCL makes the method now, as the
;;; target does once it has loaded the form.
(defun expand-defmethod-once (expansion env)
  (let ((*noted-methods* nil) (method nil))
    (prog1 `(,(car expansion)
             ,@(loop for x in (cdr expansion)
                     collect (if (typep x '(cons (eql eval-when)
                                            (cons t (cons (cons (eql %defmethod-expander)) null))))
                                 `(,(first x) ,(second x)
                                   ,(or method (setq method (sb-xc:macroexpand-1 (third x) env))))
                                 x)))
      (make-methods (let ((early (not *host-pcl-boot-seen*)))
                      (mapcar (lambda (record) (cons early record)) (reverse *noted-methods*)))))))
;;; ... and a DEFGENERIC's :METHOD options, as DEFMETHODs: its initial
;;; methods
(defvar *option-methods*)               ; (as they are made, last first)
(defun make-option-methods (form env)
  (let ((*option-methods* '()))
    (dolist (option (cdddr form))
      (when (typep option '(cons (eql :method)))
        (sb-xc:macroexpand-1 `(defmethod ,(second form) ,@(cdr option)) env)))
    (when *option-methods*
      (call-as-host-pcl
       (lambda ()
         (setf (generic-function-initial-methods (fdefinition (second form)))
               (reverse *option-methods*)))))))

(defvar *pass-2-warnings* (make-hash-table :test 'equal))
;;; Pass 2 shows what goes wrong briefly: PCL's objects are circular
(defmacro with-pass-2-reporting (&body body)
  `(let ((*print-level* 3) (*print-length* 6) (*print-circle* t) (*print-pretty* nil))
     (handler-bind ((warning
                      (lambda (c)
                        (let ((text (princ-to-string c)))
                          (unless (shiftf (gethash text *pass-2-warnings*) t)
                            (format t "~&;; pass 2 warning: ~A~%" text)))
                        (muffle-warning c)))
                    (error
                      ;; (and on to the debugger, or whatever the host does)
                      (lambda (c) (format t "~&;; pass 2 error: ~A~%" c))))
       ,@body)))

;;; As the cross-compiler begins a file (TARGET-COMPILE-STEM)
(defun sb-cold::host-pcl-before-target-compile (stem flags)
  (setq sb-xc:*macroexpand-hook* #'xc-definition-macroexpand-hook)
  (setq *host-pcl-defining* (not (member :host-definitions flags)))
  (when (string= stem "src/pcl/boot")
    (setq *host-pcl-boot-seen* t)))

;;; A function of NAME that calls its host generic function
(defun dispatch-trampoline (name) (lambda (&rest args) (apply #'dispatch-call name args)))
;;; A generic function proxy called on the host: the host's dispatch, by its name
(defvar *dispatch-routes* (make-hash-table :test 'eq)) ; record -> function or :NONE
(defun dispatch-route (record)
  (let ((route (gethash record *dispatch-routes*)))
    (when (null route)
      (setq route
            (or (and (eq (sb-kernel::layout-classoid-name (host-fin-wrapper record)) 'standard-generic-function)
                     (let ((name (svref (host-fin-slots record)
                                        (!bootstrap-slot-index 'standard-generic-function 'name))))
                       (and (gethash name *dispatch-gfs*) (dispatch-trampoline name))))
                :none))
      (setf (gethash record *dispatch-routes*) route))
    (and (functionp route) route)))


;;; CLASS's slots' initfunctions, as (location . function)s
(defun slot-initfunctions (class)
  (loop for slotd in (dispatch-call 'sb-mop:class-slots class)
        for function = (dispatch-call 'sb-mop:slot-definition-initfunction slotd)
        when function
          collect (cons (dispatch-call 'sb-mop:slot-definition-location slotd) function)))
;;; PCL's generic functions, as MAKE-INSTANCE makes them but in two steps:
;;; allocated and named first, so that each is a generic function by its
;;; name (#'INITIALIZE-INSTANCE ...) while any of them is initialized.
;;; (MAKE-INSTANCE's initarg checks need those.)  Pass 2's DEFGENERICs
;;; reinitialize them.
(defvar *pcl-gfs-made* (make-hash-table :test 'equal))
(defun make-pcl-generic-functions ()
  (let ((class (host-find-class 'standard-generic-function))
        (name-index (!bootstrap-slot-index 'standard-generic-function 'name))
        (gfs nil))
    ;; Initializing one reads others' slots (their methods, ...): all get
    ;; their slots' initial values first.
    (let ((initial (slot-initfunctions class)))
      (maphash (lambda (name entry)
                 (cond
                   ((shiftf (gethash name *pcl-gfs-made*) t)) ; made already
                   ((member name *dispatch-gfs-of-own-class* :test #'equal)
                     (fmakunbound name)) ; (PASS-2-DEFINITIONS's trampoline)
                   (t
                     (let* ((gf (dispatch-call 'allocate-instance class))
                            (slots (host-fin-slots (host-funcallable-record gf))))
                       (loop for (location . function) in initial
                             do (setf (svref slots location) (funcall function)))
                       (setf (svref slots name-index) name
                             (fdefinition name) gf)
                       (push (list* gf name (third entry)) gfs)))))
               *dispatch-gfs*))
    (dolist (entry (reverse (shiftf *pass-2-capturing-forms* nil)))
      (let ((*package* (car entry))) (eval (cdr entry))))
    ;; (what they cache of COMPUTE-APPLICABLE-METHODS, which has no methods
    ;; yet, RUN-PASS-2 computes again)
    (let ((defaults (dispatch-call 'sb-mop:class-default-initargs class)))
      (loop for (gf name . lambda-list) in gfs
            do (apply #'dispatch-call 'initialize-instance gf
                      (default-initargs (list* :name name
                                               (and lambda-list (list :lambda-list lambda-list)))
                                        defaults))))))

;;; The accessors of the classes of the meta-braid, as DEFCLASS makes them
;;; (FIX-SLOT-ACCESSORS)
(defun make-braid-accessors ()
  (dolist (ecd *!early-class-definitions*)
    (unless (member (ecd-metaclass ecd) '(built-in-class system-class))
      (let ((class (host-find-class (ecd-class-name ecd))))
        (flet ((gf (name lambda-list)
                 (or (find-generic-function name nil)
                     (funcall 'ensure-generic-function
                              name :lambda-list lambda-list))))
          (dolist (slot (ecd-canonical-slots ecd))
            (dolist (reader (getf slot :readers))
              (add-reader-method class (gf reader '(object)) (getf slot :name)
                                 nil (ecd-source-location ecd)))
            (dolist (writer (getf slot :writers))
              (add-writer-method class (gf writer '(new-value object)) (getf slot :name)
                                 nil (ecd-source-location ecd)))))))))

;;; Methods for the host only: FIND-METHOD-COMBINATION's answers for what
;;; host PCL makes before defcombin.lisp's table of method combinations has
;;; them.  STANDARD and OR are the meta-braid's (class-init.lisp).
(defun host-only-methods ()
  (dispatch-class 'generic-function)
  ;; (:AROUND defcombin.lisp's, whose table has them once its toplevel
  ;; forms have run)
  (eval `(cl:defmethod ,(dispatch-symbol 'find-method-combination) :around
             ((k0 ,(dispatch-symbol 'generic-function)) k1 k2 &rest args)
           (declare (ignore k0 k1 k2))
           (destructuring-bind (gf name options) args
             (declare (ignore gf))
             (or (and (next-method-p) (call-next-method))
                 (and (null options)
                      (cond ((string= name "STANDARD") *standard-method-combination*)
                            ((string= name "OR") *or-method-combination*)))
                 (error "host PCL: no method combination ~S ~S" name options))))))

;;; The definitions recorded since: another file's, or, if PROPER,
;;; PCL proper's (HOST-PCL-BUILD-PROPER), with the cross-compiler
(defun pass-2-definitions (&optional proper)
  (build-dispatch)
  (when proper
    (host-only-methods)
    (setq *host-pcl-ready* t))
  (with-pass-2-reporting
    ;; until they are made: PCL's generic functions' names call the host's dispatch
    (maphash (lambda (name entry)
               (declare (ignore entry))
               (unless (gethash name *pcl-gfs-made*)
                 (setf (fdefinition name) (dispatch-trampoline name))))
             *dispatch-gfs*)
    (make-pcl-generic-functions)
    (when proper (make-braid-accessors))
    (dolist (entry (reverse (shiftf *pass-2-forms* nil)))
      (let ((*package* (car entry))) (eval (cdr entry))))))
;;; ... and the rest, in genesis
(defun run-pass-2 ()
  (setq *host-pcl-ready* t)
  (format t "~&; host PCL dispatch: ~D generic functions, ~D dispatch classes~%"
          (hash-table-count *dispatch-gfs*) (hash-table-count *dispatch-classes*))
  (with-pass-2-reporting
    (redefine-braid-classes)
    (make-structure-classes)
    (finalize-compile-time-classes)
    (make-methods (shiftf *methods-waiting* nil) nil)
    (format t "~&; host PCL: ~D methods made of what the cross-compiler noted~%" *methods-made*)
    ;; what each generic function caches of COMPUTE-APPLICABLE-METHODS, now
    ;; that it has its methods
    (maphash (lambda (name entry)
               (declare (ignore entry))
               (let ((gf (fdefinition name)))
                 (when (funcall 'generic-function-p gf)
                   (update-c-a-m-gf-info gf))))
             *dispatch-gfs*)
    ;; SEQUENCE's prototype, which is not in *BUILT-IN-CLASSES*
    (eval '(setf (slot-value (find-class 'sequence) 'sb-pcl::prototype) #()))
    ;; ctor.lisp's, which it sets on first use, its generic functions not
    ;; existing when PCL is built (except on the host)
    (eval '(setq sb-pcl::*the-system-si-method*
                  (find-method #'shared-initialize
                               () (list sb-pcl::*the-class-slot-object* sb-pcl::*the-class-t*))
                  sb-pcl::*the-system-ii-method*
                  (find-method #'initialize-instance
                               () (list sb-pcl::*the-class-slot-object*))))
    (make-dfuns)
    (make-cold-init-dfuns)))

;;; Every generic function's discriminating function, as PCL makes it once
;;; it is booted (COMPUTE-DISCRIMINATING-FUNCTION: the final one of PCL's
;;; own, the initial one of the others): PCL is bootstrapped on the host,
;;; and genesis writes them
(defun make-dfuns ()
  (let ((generic-function-p 'generic-function-p)
        (n 0))
    (maphash (lambda (closure record)
               ;; (not the classes' prototypes, say)
               (when (and (host-fin-p record) (funcall generic-function-p closure)
                          (let* ((slots (host-fin-slots record))
                                 (index (!bootstrap-slot-index 'standard-generic-function
                                                               'name))
                                 (name (and (< index (length slots)) (svref slots index))))
                            (and (or (symbolp name) (consp name)) (fboundp name)
                                 (eq (fdefinition name) closure))))
                 (update-dfun closure)
                 (incf n)))
             *host-funcallables*)
    (format t "~&; host PCL: discriminating functions of ~D generic functions~%" n)))

;;; The functions of the methods host PCL makes -> the names of their
;;; cross-compiled ones (SB-FASL::*COLD-PCL-FUNCTIONS*)
(defvar *early-method-function-keys* (make-hash-table :test 'eq))

;;; The generic functions of the methods of files before PCL's boot.lisp
;;; (PRINT-OBJECT, CLOSE, ...), which cold init calls before PCL's toplevel
;;; forms
(defvar *early-method-gfs* nil)

;;; Methods, as real methods, of what the cross-compiler noted as it compiled
;;; their DEFMETHODs (NOTE-METHOD-INFO): their lambda lists, source
;;; locations and initargs.  Their functions are the cross-compiled ones
;;; (HP-DUMP-OBJECT in host-pcl-dump.lisp finds them by the name of
;;; the host function standing in for each).  A method of a generic
;;; function defined later (by a DEFGENERIC of a file after it) waits for
;;; it, if WAIT.
(defun make-methods (records &optional (wait t))
  (call-as-host-pcl
   (lambda ()
     (with-pass-2-reporting
       (dolist (record records)       ; (early fun-name lambda-list location initargs)
         (cond ((make-noted-method record))
               (wait (setq *methods-waiting* (nconc *methods-waiting* (list record))))
               (t (error "host PCL: no generic function for the method ~S" (second record)))))))))
;;; ... one, unless its generic function is not defined yet
(defun make-noted-method (record)
  (destructuring-bind (early fun-name lambda-list location initargs) record
    (destructuring-bind (name &rest rest) (copy-tree (cdr fun-name)) ; (fast-method name ...)
      (let ((qualifiers (butlast rest)) (specializers (car (last rest)))
            (gf (and (fboundp name) (fdefinition name))))
        (when (and gf (funcall 'generic-function-p gf))
          (incf *methods-made*)
          (when early
            (pushnew name *early-method-gfs* :test #'equal))
          (let* ((key fun-name)
                 ;; (a closure, so each method's is its own)
                 (fmf (lambda (&rest args)
                        (declare (ignore args))
                        (error "host PCL: the cross-compiled function of ~S" key)))
                 (method
                   (progn
                     (setf (gethash fmf *early-method-function-keys*) key)
                     (funcall 'sb-pcl::!install-cold-method
                              name qualifiers specializers fmf lambda-list location initargs))))
            (when (boundp '*option-methods*)
              (push method *option-methods*))
            t))))))

;;; Finalized as made, as the target's would be before it makes methods'
;;; caches (lazy finalization would invalidate the layouts cached)
(defun finalize-compile-time-classes ()
  (let ((finalize (fdefinition 'sb-mop:finalize-inheritance)))
    (dolist (name (reverse *compile-time-class-names*))
      (funcall finalize (host-find-class name)))))

;;; Their discriminating functions: caches for every class host PCL knows,
;;; or dispatch nets (on EQL specializers) of all their methods: cold init prints (its first is (WRITE 'T)) and uses streams before
;;; the type system, the classoids or PCL's toplevel forms are set up, and a
;;; cache hit needs only the argument's layout.  A miss (a class made at run
;;; time) can only be handled once PCL's toplevel forms have run.
(defun make-cold-init-dfuns ()
  (let* ((finalized-p 'sb-mop:class-finalized-p)
         (classes (loop for (nil . class) in (host-pcl-classes)
                        when (funcall finalized-p class)
                          collect (list class))))
    (dolist (name *early-method-gfs*)
      (let ((gf (fdefinition name)))
        (multiple-value-bind (dfun cache info)
            (if (= (nth-value 3 (get-generic-fun-info gf)) 1)
                (make-final-dfun-internal gf classes)
                ;; DOCUMENTATION's dispatch on EQL specializers, which a cache
                ;; of classes cannot tell apart: the dispatch net of all its
                ;; methods
                (make-dispatch-dfun gf))
          (update-dfun gf dfun cache info))))
    (format t "~&; host PCL: caches for ~D classes~%" (length classes))))

;;; The classes of the meta-braid (class-init.lisp), made again by PCL's
;;; protocol as DEFCLASS would: they keep their wrappers (%UPDATE-SLOTS)
;;; when the slots come out as the braid laid them out, so what was made
;;; with the braid's stays valid.  Anything else is an error.
(defun fill-unbound-slots (object)
  (let ((slots (host-instance-slots object)))
    (loop for (location . function)
            in (slot-initfunctions (wrapper-class (host-instance-wrapper object)))
          when (and (integerp location) (eq (svref slots location) +slot-unbound+))
            do (setf (svref slots location) (funcall function)))))

(defvar *redefining-braid* nil)

(defun redefine-braid-classes ()
  ;; The braid's superclasses are PCL's design (SLOT-OBJECT, a SLOT-CLASS,
  ;; is STANDARD-OBJECT's), which its own rules would not allow.
  (eval `(cl:defmethod ,(dispatch-symbol 'sb-mop:validate-superclass) :around
             (k0 k1 &rest args)
           (declare (ignore k0 k1 args))
           (or *redefining-braid* (call-next-method))))
  (let ((*redefining-braid* t))
    (redefine-braid-classes-1)))

(defun redefine-braid-classes-1 ()
  ;; the braid's slot definitions lack what their initforms give
  (dolist (ecd *!early-class-definitions*)
    (when (member (ecd-metaclass ecd) '(standard-class funcallable-standard-class))
      (dolist (slotd (dispatch-call 'sb-mop:class-direct-slots
                                    (host-find-class (ecd-class-name ecd))))
        (fill-unbound-slots slotd))))
  (let ((redefined 0))
    (dolist (ecd *!early-class-definitions*)
      (when (member (ecd-metaclass ecd) '(standard-class funcallable-standard-class))
        (let* ((name (ecd-class-name ecd))
               (class (host-find-class name))
               (wrapper (class-wrapper class)))
          (if (eq name (ecd-metaclass ecd))
              ;; STANDARD-CLASS, which ENSURE-CLASS refuses to make its own
              ;; metaclass: what ENSURE-CLASS-USING-CLASS would do
              (apply #'dispatch-call 'reinitialize-instance class
                     :direct-superclasses (mapcar #'host-find-class (ecd-superclass-names ecd))
                     :direct-slots (ecd-canonical-slots ecd)
                     'source (ecd-source-location ecd)
                     (ecd-other-initargs ecd))
              (apply 'sb-mop:ensure-class name
                     :metaclass (ecd-metaclass ecd)
                     :direct-superclasses (ecd-superclass-names ecd)
                     :direct-slots (ecd-canonical-slots ecd)
                     'source (ecd-source-location ecd)
                     (ecd-other-initargs ecd)))
          (unless (and (eq (host-find-class name) class) (eq (class-wrapper class) wrapper))
            (error "host PCL: ~S came out unlike the meta-braid's" name))
          (incf redefined))))
    ;; what the braid set to its slot definitions (std-class.lisp)
    (setq *the-eslotd-standard-class-slots*
          (find-slot-definition (host-find-class 'standard-class) 'slots)
          *the-eslotd-funcallable-standard-class-slots*
          (find-slot-definition (host-find-class 'funcallable-standard-class) 'slots))
    (format t "~&; host PCL: ~D classes of the meta-braid made again by PCL's protocol~%"
            redefined)))

;;; The classes of the structures and conditions the cross-compiler knows,
;;; as fixup.lisp's sweep makes them in cold init: those of PCL proper's
;;; time as make-host-1 builds host PCL, and the rest as make-host-2 ends
(defvar *structure-classes-made* 0)
(defun make-structure-classes (&optional (report t))
  ;; (conditions' slots are cold init's: fixup.lisp's sweep fills them in)
  (dolist (root '(sb-kernel::condition function structure-object))
    (sb-kernel::do-subclassoids ((classoid layout) (sb-kernel:find-classoid root))
      (declare (ignore layout))
      (unless (sb-kernel:classoid-pcl-class classoid)
        (ensure-non-standard-class (sb-kernel:classoid-name classoid) classoid)
        (incf *structure-classes-made*))))
  (when report
    (format t "~&; host PCL: ~D structure and condition classes made~%" *structure-classes-made*)))

;;;; Before host PCL boots its meta-braid (class-init.lisp)

;;; The classoids of the meta-braid's classes, as the cross-compiler sees
;;; them once defs.lisp has made their early definitions: their names are
;;; types it knows, with their superclasses, for TYPEP and type inference.
;;; Host PCL gives them their classes and layouts (host-pcl.lisp).
(labels ((layout-named (name)
           (let ((classoid (find-classoid name nil)))
             (if classoid
                 (classoid-layout classoid)
                 (let* ((supers (mapcar #'layout-named
                                        (ecd-superclass-names (!early-class-definition name))))
                        (inherits (remove-duplicates
                                   (loop for super in supers
                                         append (coerce (layout-inherits super) 'list)
                                         collect super)
                                   :from-end t))
                        (classoid (make-standard-classoid :name name))
                        (layout (make-layout (hash-layout-name name) classoid
                                             :depthoid -1 :length 0 :invalid nil
                                             :inherits (coerce inherits 'simple-vector))))
                   (register-layout layout :invalidate nil) ; (its subclassoids)
                   (setf (classoid-cell-classoid (find-classoid-cell name :create t)) classoid
                         (info :type :kind name) :instance)
                   layout)))))
  (dolist (ecd *!early-class-definitions*)
    (layout-named (ecd-class-name ecd))))
;;; what the cross-compiler's copy of defs.lisp made
(host-pcl-built-in-classes)
(setq *host-pcl-variables-before* (host-pcl-variables))
(install-host-pcl-boot-operators)
