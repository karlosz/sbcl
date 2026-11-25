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
(declaim (special sb-cold::*host-pcl-record* sb-fasl::*cold-early-methods*)
         (ftype function add-reader-method add-writer-method class-wrapper))

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
    ;; An EQL specializer (defcombin.lisp's on *STANDARD-METHOD-COMBINATION*):
    ;; the host's dispatch is on classes, and its bootstrap needs no such
    ;; method; the method itself is PCL's, made in pass 2 as any other
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
;;; (src/cold/host-pcl-defs.lisp)
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

(defun build-dispatch ()
  (let ((failures 0))
    (maphash #'make-dispatch-gf *dispatch-gfs*)
    (dolist (form (reverse *dispatch-forms*))
      (handler-case (eval form)
        (error (c)
          (incf failures)
          (format t "~&; host PCL dispatch: ~A~%" c))))
    (format t "~&; host PCL dispatch: ~D generic functions, ~D dispatch classes, ~D failures~%"
            (hash-table-count *dispatch-gfs*) (hash-table-count *dispatch-classes*) failures)))

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

;;; The forms that pass 2 runs, as the cross-compiler recorded them in build
;;; order: PCL proper's files' (HOST-PCL-PROPER-STEMS); and a target-only
;;; file's classes and generic functions, whose methods host PCL makes from
;;; what genesis has of them (MAKE-EARLY-METHODS)
(defun take-pass-2-forms ()
  (let ((proper (host-pcl-proper-stems))
        (target-only (host-pcl-target-only-stems)))
    (dolist (entry (reverse sb-cold::*host-pcl-record*))
      (destructuring-bind (kind stem package form) entry
        (when (or (member stem proper :test #'string=)
                  (and (member stem target-only :test #'string=)
                       (member kind '(:class :generic-function))))
          (note-dispatch-form kind form)
          (when (and (eq kind :class) (member stem target-only :test #'string=))
            (push (second form) *target-only-class-names*))
          (if (eq kind :capturing)
              (push (cons package form) *pass-2-capturing-forms*)
              (push (cons package form) *pass-2-forms*)))))))

;;; A generic function proxy called on the host: the host's dispatch, by its name
(defvar *dispatch-routes* (make-hash-table :test 'eq)) ; record -> function or :NONE
(defun dispatch-route (record)
  (let ((route (gethash record *dispatch-routes*)))
    (when (null route)
      (setq route
            (or (and (eq (sb-kernel::layout-classoid-name (host-fin-wrapper record)) 'standard-generic-function)
                     (let ((name (svref (host-fin-slots record)
                                        (!bootstrap-slot-index 'standard-generic-function 'name))))
                       (and (gethash name *dispatch-gfs*)
                            (lambda (&rest args) (apply #'dispatch-call name args)))))
                :none))
      (setf (gethash record *dispatch-routes*) route))
    (and (functionp route) route)))

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
                      (lambda (c)
                        (format t "~&;; pass 2 error: ~A~%" c)
                        (host-sb-debug:print-backtrace :count 30)
                        (host-sb-ext:exit :code 4 :abort t)))
                    (storage-condition
                      (lambda (c)
                        (declare (ignore c))
                        (host-sb-debug:print-backtrace :count 80)
                        (host-sb-ext:exit :code 3 :abort t))))
       ,@body)))

;;; PCL's generic functions, as MAKE-INSTANCE makes them but in two steps:
;;; allocated and named first, so that each is a generic function by its
;;; name (#'INITIALIZE-INSTANCE ...) while any of them is initialized.
;;; (MAKE-INSTANCE's initarg checks need those.)  Pass 2's DEFGENERICs
;;; reinitialize them.
(defun make-pcl-generic-functions ()
  (let ((class (host-find-class 'standard-generic-function))
        (name-index (!bootstrap-slot-index 'standard-generic-function 'name))
        (gfs nil))
    ;; Initializing one reads others' slots (their methods, ...): all get
    ;; their slots' initial values first.
    (let ((initial (loop for slotd in (dispatch-call 'sb-mop:class-slots class)
                         for function = (dispatch-call
                                         'sb-mop:slot-definition-initfunction slotd)
                         when function
                           collect (cons (dispatch-call
                                          'sb-mop:slot-definition-location slotd)
                                         function))))
      (maphash (lambda (name entry)
                 (if (member name *dispatch-gfs-of-own-class* :test #'equal)
                     (fmakunbound name) ; (RUN-PASS-2's trampoline)
                     (let* ((gf (dispatch-call 'allocate-instance class))
                            (slots (host-fin-slots (host-funcallable-record gf))))
                       (loop for (location . function) in initial
                             do (setf (svref slots location) (funcall function)))
                       (setf (svref slots name-index) name
                             (fdefinition name) gf)
                       (push (list* gf name (third entry)) gfs))))
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

(defun run-pass-2 ()
  (build-dispatch)
  (host-only-methods)
  (setq *host-dispatch-routing* t
        *host-pcl-ready* t)
  (with-pass-2-reporting
    ;; until they are made: PCL's generic functions' names call the host's dispatch
    (maphash (lambda (name entry)
               (declare (ignore entry))
               (setf (fdefinition name) (lambda (&rest args) (apply #'dispatch-call name args))))
             *dispatch-gfs*)
    (make-pcl-generic-functions)
    (make-braid-accessors)
    (dolist (entry (reverse (shiftf *pass-2-forms* nil)))
      (let ((*package* (car entry))) (eval (cdr entry))))
    (redefine-braid-classes)
    (make-structure-classes)
    (finalize-target-only-classes)
    (make-early-methods)
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

;;; The functions of the methods made of what genesis has -> the keys of
;;; their cross-compiled ones (SB-FASL::*COLD-PCL-METHOD-FUNCTIONS*)
(defvar *early-method-function-keys* (make-hash-table :test 'eq))

;;; The generic functions of the methods of files before PCL's boot.lisp
;;; (PRINT-OBJECT, CLOSE, ...), which cold init calls before PCL's toplevel
;;; forms
(defvar *early-method-gfs* nil)

;;; The methods of the files before PCL's boot.lisp, and those of the files
;;; whose bodies are the target's only (*HOST-PCL-TARGET-ONLY*), as real
;;; methods:
;;; genesis has their lambda lists and source locations (FOP-MSET), and
;;; their functions are the cross-compiled ones (HOST-PCL-COLD-FUNCTION in
;;; host-pcl-dump.lisp finds them by the name of the host function standing
;;; in for each).
(defun make-early-methods ()
  (dolist (method (reverse sb-fasl::*cold-early-methods*))
    (destructuring-bind (name qualifiers specializers lambda-list source initargs early-p)
        method
      (let ((host-name name))
        (unless (and (fboundp host-name)
                     (funcall 'generic-function-p
                              (fdefinition host-name)))
          (error "host PCL: no generic function ~S for the early method ~S ~S"
                 name qualifiers specializers))
        (when early-p
          (pushnew host-name *early-method-gfs* :test #'equal))
        ;; its function, which stands for the cross-compiled one genesis has
        ;; (HOST-PCL-COLD-FUNCTION)
        (let* ((key (append (list name) qualifiers (list specializers)))
               ;; (a closure, so each method's is its own)
               (fmf (lambda (&rest args)
                      (declare (ignore args))
                      (error "host PCL: the cross-compiled function of ~S" key))))
          (setf (gethash fmf *early-method-function-keys*) key)
          (funcall 'sb-pcl::!install-cold-method
                   (host-pcl-canonical name) (host-pcl-canonical qualifiers)
                   (host-pcl-canonical specializers) fmf (host-pcl-canonical lambda-list)
                   source (host-pcl-canonical initargs))))))
  (format t "~&; host PCL: ~D methods made of what genesis has~%"
          (length sb-fasl::*cold-early-methods*)))

;;; Finalized as made, as the target's would be before it makes methods'
;;; caches (lazy finalization would invalidate the layouts cached)
(defun finalize-target-only-classes ()
  (let ((finalize (fdefinition 'sb-mop:finalize-inheritance)))
    (dolist (name (reverse *target-only-class-names*))
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
    (dolist (slotd (dispatch-call 'sb-mop:class-slots
                                  (wrapper-class (host-instance-wrapper object))))
      (let ((location (dispatch-call 'sb-mop:slot-definition-location slotd))
            (function (dispatch-call 'sb-mop:slot-definition-initfunction slotd)))
        (when (and (integerp location) function
                   (eq (svref slots location) +slot-unbound+))
          (setf (svref slots location) (funcall function)))))))

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

;;; EXPERIMENT: the classes of the structures and conditions the
;;; cross-compiler knows, as fixup.lisp's sweep makes them in cold init
(defun make-structure-classes ()
  (let ((made 0) (failed (make-hash-table :test 'equal)))
    ;; (conditions' slots are cold init's: fixup.lisp's sweep fills them in)
    (dolist (root '(sb-kernel::condition function structure-object))
      (sb-kernel::do-subclassoids ((classoid layout) (sb-kernel:find-classoid root))
        (declare (ignore layout))
        (unless (sb-kernel:classoid-pcl-class classoid)
          (handler-case
              (progn (ensure-non-standard-class (sb-kernel:classoid-name classoid) classoid)
                     (incf made))
            (error (c)
              (push (sb-kernel:classoid-name classoid)
                    (gethash (let ((s (princ-to-string c))) (subseq s 0 (min 90 (length s))))
                             failed)))))))
    (format t "~&; host PCL: ~D structure and condition classes made~%" made)
    (maphash (lambda (text names)
               (format t "~&;   ~D failed: ~A (~{~S~^ ~})~%" (length names) text
                       (subseq names 0 (min 4 (length names)))))
             failed)))
