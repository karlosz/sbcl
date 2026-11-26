;;;; Running PCL's own bootstrap code on the host, so that genesis can dump
;;;; the objects it builds.
;;;;
;;;; PCL's construction code (src/pcl/ecd.lisp, defs.lisp, class-init.lisp) is
;;;; loaded into the host unmodified, over a small substrate:
;;;;
;;;;  - PCL objects are host proxies: HOST-INSTANCE (a standard instance),
;;;;    a host closure with a HOST-FIN record (a funcallable standard
;;;;    instance, callable on the host like the target's) and HOST-WRAPPER (a
;;;;    layout of a PCL class).  Genesis turns them into the corresponding target
;;;;    objects without knowing anything about PCL.
;;;;
;;;;  - Whatever must be made by the target itself (closures, anything that
;;;;    depends on target hash codes, prototypes of structure and condition
;;;;    classes) is a HOST-RECIPE: a call of a target function on dumped
;;;;    arguments, which cold init performs where the object is needed.
;;;;
;;;;  - It lives in SB-PCL, and runs the cross-compiler's copy of PCL's files
;;;;    (:HOST-DEFINITIONS) with what it loads of them.  Other packages'
;;;;    names of PCL's generic functions are read as shadows
;;;;    (HOST-PCL-MAKE-SHADOWS).

(in-package "SB-PCL")

;;; genesis's (FOP-MSET), and what INSTALL-HOST-PCL-OPERATORS defines
(declaim (special sb-fasl::*cold-method-records*)
         (ftype function host-real-load-defmethod))

;;; Non-PCL target functions that PCL's macro expanders call.
(unless (fboundp 'sb-int:interned-symbol-p)
  (defun sb-int:interned-symbol-p (x) (and (symbolp x) (cl:symbol-package x) t)))
(unless (fboundp 'sb-int:posq)
  (defun sb-int:posq (item list) (position item list :test #'eq)))
;;; In SB-PCL some CL names are the cross-compiler's strict versions, which
;;; it leaves undefined (set-up-cold-packages): PCL's code has the host's.
(dolist (name '("SYMBOL-PACKAGE" "PACKAGE-NAME"))
  (let ((s (find-symbol name "XC-STRICT-CL")))
    (when (and s (not (fboundp s)))
      (setf (fdefinition s) (fdefinition (find-symbol name "CL"))))))



;;;; Proxies (their structures are the cross-compiler's: cross-pcl.lisp)

;;; On, generic functions called on the host are dispatched by the host's CLOS
;;; (host-pcl-dispatch.lisp); their functions are for the target only.
(defvar *host-dispatch-routing* nil)
(defun host-make-funcallable (record)
  (let ((closure (lambda (&rest args)
                   (apply (or (and *host-dispatch-routing* (host-fin-p record)
                                   (dispatch-route record))
                              (host-funcallable-function record)
                              (error "host PCL: funcallable object without a function"))
                          args))))
    (setf (gethash closure *host-funcallables*) record)
    closure))

;;; !DEFSTRUCT-WITH-ALTERNATE-METACLASS: CTOR and %METHOD-FUNCTION
(defmacro host-funcallable-structure (name &key slot-names constructor &allow-other-keys)
  (let ((record (sb-int:symbolicate "HOST-" name "-RECORD"))
        (make (sb-int:symbolicate "%MAKE-HOST-" name "-RECORD"))
        (predicate (sb-int:symbolicate name "-P")))
    `(progn
       ;; the target's structure, as the cross-compiler knows it once it
       ;; compiles its DEFSTRUCT (!DEFSTRUCT-WITH-ALTERNATE-METACLASS's
       ;; compile-time effect): host PCL builds before then
       (let ((dd (sb-kernel::make-dd-with-alternate-metaclass
                  :class-name ',name :slot-names ',slot-names
                  :superclass-name 'function :metaclass-name 'sb-kernel:static-classoid
                  :metaclass-constructor 'sb-kernel::make-static-classoid
                  :dd-type 'sb-kernel:funcallable-structure)))
         (sb-kernel::%compiler-defstruct dd (sb-kernel::!inherits-for-structure dd)))
       (defstruct (,record (:include host-funcallable)
                           (:constructor ,make ,slot-names))
         ,@slot-names)
       (setf (get ',record 'host-structure-name) ',name)
       (defun ,constructor ,slot-names (host-make-funcallable (,make ,@slot-names)))
       (defun ,predicate (x) (,(sb-int:symbolicate record "-P") (host-funcallable-record x)))
       (deftype ,name () '(satisfies ,predicate))
       ,@(loop for slot in slot-names
               for accessor = (sb-int:symbolicate name "-" slot)
               for reader = (sb-int:symbolicate record "-" slot)
               collect `(defun ,accessor (x) (,reader (host-funcallable-record x)))
               collect `(defun (setf ,accessor) (new x)
                          (setf (,reader (host-funcallable-record x)) new))))))

;;; A layout of a PCL class: the cross-compiler's LAYOUT structure, made by
;;; host PCL (HOST-MAKE-LAYOUT)
(defvar *host-pcl-layouts* (make-hash-table :test 'eq))
(defvar *host-pcl-classoids* (make-hash-table :test 'eq)) ; theirs
(defun host-wrapper-p (x) (and (gethash x *host-pcl-layouts*) t))

;;; FUNCTION (a symbol naming a target function) of ARGS: what the host has
;;; in place of an object only the target can make.  Genesis writes none.
(defstruct (host-recipe (:constructor make-host-recipe (function &rest args)))
  function args)

(defvar *host-wrappers* (make-hash-table :test 'eq))  ; name -> wrapper proxy



;;; pre-warm.lisp's and ctor.lisp's, which are the target's
(host-funcallable-structure %method-function
  :slot-names (fast-function) :constructor %make-method-function)
(host-funcallable-structure ctor
  :slot-names (type class-or-name class initargs state safe-p) :constructor %make-ctor)

;;;; The substrate.  These definitions replace the target ones (low.lisp,
;;;; wrapper.lisp and the allocation functions of class-init.lisp).  (Of
;;;; low.lisp's operators on instances, the cross-compiler's copy works on
;;;; proxies: src/code/cross-pcl.lisp.)

(defun host-pcl-instance-p (x) (or (host-instance-p x) (and (host-fin-record x) t)))
;;; Function names are the target's business; the host keeps them for the
;;; record only.
(defun set-fun-name (fun new-name)
  (setf (gethash fun *host-fun-names*) new-name)
  fun)

(defun !boot-make-wrapper (length name &optional (bitmap sb-kernel::+layout-all-tagged+))
  ;; As in wrapper.lisp, a class whose classoid exists already (T, FUNCTION,
  ;; CONDITION, STRUCTURE-OBJECT, ...) uses that classoid's layout.  The
  ;; cross-compiler's classoid of a class of the meta-braid (class-init.lisp),
  ;; which cross-compiled code may refer to, becomes the class's.
  (let ((found (sb-kernel:find-classoid name nil)))
    (when (and found (not (typep found 'sb-kernel:standard-classoid)))
      (return-from !boot-make-wrapper (sb-kernel:classoid-layout found))))
  (or (gethash name *host-wrappers*)
      ;; its type, to the host (cross-pcl.lisp, which has CL's names' already)
      (progn (unless (or (eq (cl:symbol-package name) (find-package "SB-XC"))
                         (cl:find-class name nil))
               (define-host-pcl-class-type name))
             nil)
      ;; like the target's: a fresh STANDARD-CLASSOID, not entered in its cell
      (setf (gethash name *host-wrappers*)
            (host-make-layout nil (or (sb-kernel:find-classoid name nil)
                                      (sb-kernel:make-standard-classoid :name name))
                              :length length :bitmap bitmap :invalid nil
                              ;; as MAKE-LAYOUT is called in wrapper.lisp
                              :flags (logior sb-kernel::+pcl-object-layout-flag+
                                             sb-kernel::+strictly-boxed-flag+)))))

;;; wrapper.lisp's MAKE-LAYOUT and MAKE-STANDARD-CLASSOID calls
(defun host-make-layout (hash classoid &key (length 0) (flags 0)
                                            (bitmap sb-kernel::+layout-all-tagged+)
                                            invalid (depthoid -1) (inherits #()))
  (declare (ignore hash))
  (let ((w (sb-kernel::host-make-layout
            nil
            ;; the target layout's (genesis's MAKE-COLD-LAYOUT), so that
            ;; dispatch caches are the same on the target
            (sb-impl::hash-layout-name (sb-kernel:classoid-name classoid))
            classoid :length length :invalid invalid :depthoid depthoid :inherits inherits)))
    (setf (sb-kernel::layout-%flags w) flags
          (sb-kernel::layout-%bitmap w) bitmap
          (gethash w *host-pcl-layouts*) t
          (gethash classoid *host-pcl-classoids*) t)
    w))
;;; REGISTER-LAYOUT, the cross-compiler's on the host being the target's in
;;; this: re-registering a layout of the meta-braid (which only host PCL
;;; does) invalidates the old one only, not its subclasses'
(defun host-register-layout (layout &key (invalidate t) destruct-layout)
  (declare (ignore destruct-layout))
  (let* ((classoid (sb-kernel:layout-classoid layout))
         (old (sb-kernel:classoid-layout classoid)))
    (aver (host-own-classoid-p classoid))
    (when (and old invalidate) (setf (sb-kernel:layout-invalid old) t))
    (setf (sb-kernel:classoid-layout classoid) layout)))
;;; A classoid of PCL's layouts: one PCL made, or one of the cross-compiler's
;;; that it took (class-init.lisp)
(defun host-own-classoid-p (classoid)
  (and (gethash classoid *host-pcl-classoids*) t))

(defun allocate-standard-funcallable-instance (wrapper name)
  (declare (ignore name))
  (let* ((record (%make-host-fin wrapper (make-array (sb-kernel:layout-length wrapper)
                                                     :initial-element +slot-unbound+)
                                 ;; SXHASH: as quasi-random as the target's
                                 (1+ (random (ash 1 (- sb-vm:n-fixnum-bits 2))))
                                 nil))
         (fin (host-make-funcallable record)))
    (setf (host-fin-target-function record) (!unset-funcallable-instance-function fin))
    fin))

(defun host-find-class (name &optional (errorp t) environment)
  (declare (ignore environment))
  (or (let ((cell (sb-kernel:find-classoid-cell name)))
        (and cell (sb-kernel:classoid-cell-pcl-class cell)))
      ;; as FIND-CLASS-FROM-CELL: structure and condition classes are made
      ;; when first asked for
      (and (boundp '*create-classes-from-internal-structure-definitions-p*)
           *create-classes-from-internal-structure-definitions-p*
           ;; (until host PCL builds the rest of the hierarchy, the
           ;; cross-compiler's: make-host-2 compiles with it)
           (or *host-pcl-ready*
               (return-from host-find-class (cl:find-class name errorp)))
           (let ((classoid (sb-kernel:find-classoid name nil)))
             (and classoid
                  (or (sb-kernel::condition-classoid-p classoid)
                      (defstruct-classoid-p classoid))
                  (ensure-non-standard-class name classoid))))
      (and errorp (error "host PCL: no class named ~S" name))))
(defun (setf host-find-class) (class name &optional errorp environment)
  (declare (ignore errorp environment))
  (setf (sb-kernel:classoid-cell-pcl-class (sb-kernel:find-classoid-cell name :create t))
        class))
;;; PCL's classes, as (name . class)
;;; Whether host PCL has the class NAME (pre-warm.lisp's !DEFINITION)
(defun !host-pcl-class-p (name)
  (let ((cell (sb-kernel:find-classoid-cell name)))
    (and cell (sb-kernel:classoid-cell-pcl-class cell) t)))
(defun host-pcl-classes ()
  (let ((cells nil))
    ;; (a cell is SB-XC's name's and its CL namesake's)
    (do-all-symbols (s)
      (let ((cell (sb-int:info :type :classoid-cell s)))
        (when (and cell (sb-kernel:classoid-cell-pcl-class cell))
          (pushnew cell cells))))
    (mapcar (lambda (cell)
              (cons (sb-kernel::classoid-cell-name cell) (sb-kernel:classoid-cell-pcl-class cell)))
            cells)))


;;; TEMPLATE-LAMBDA (low.lisp): genesis makes the target's closure from the
;;; template's function and the values of its variables.
(defvar *host-templates* (make-hash-table :test 'eq)) ; closure -> (name . alist)
(defun host-template (name values closure)
  (setf (gethash closure *host-templates*) (cons name values))
  closure)

;;; A method combination's set of generic functions; the target's hashes
;;; them, so genesis writes a list, made into the set when first needed
;;; (METHOD-COMBINATION-GF-SET).
(defstruct (host-gf-hashset (:constructor make-gf-hashset ())) (members nil))
(defun host-add-to-weak-hashset (key set) (pushnew key (host-gf-hashset-members set)))
(defun host-remove-from-weak-hashset (key set)
  (setf (host-gf-hashset-members set) (remove key (host-gf-hashset-members set))))
(defun host-weak-hashset-memberp (key set) (member key (host-gf-hashset-members set)))
(defun |STRUCTURE-OBJECT class constructor| ()
  (make-host-recipe '|STRUCTURE-OBJECT class constructor|))
(defun host-make-condition (name) (make-host-recipe 'make-condition name))

;;; Operators of the cross-compiler's that are host PCL's from when it boots
;;; (class-init.lisp): FIND-CLASS, which answers as the cross-compiler's for
;;; what host PCL has not made (make-host-2 compiles with it), and the layouts
;;; of PCL's classes
(defun install-host-pcl-boot-operators ()
  (setf (fdefinition 'sb-xc:find-class) #'host-find-class
        (fdefinition '(setf sb-xc:find-class)) #'(setf host-find-class))
  (let ((make-layout (fdefinition 'sb-kernel:make-layout))
        (register-layout (fdefinition 'sb-kernel:register-layout)))
    (setf (fdefinition 'sb-kernel:make-layout)
          (lambda (hash classoid &rest keys)
            (if (typep classoid 'sb-kernel:standard-classoid) ; PCL's
                (apply #'host-make-layout hash classoid keys)
                (apply make-layout hash classoid keys)))
          (fdefinition 'sb-kernel:register-layout)
          (lambda (layout &rest keys)
            (if (host-own-classoid-p (sb-kernel:layout-classoid layout))
                (apply #'host-register-layout layout keys)
                (apply register-layout layout keys)))))
  (setq *host-pcl-class-typep* #'host-pcl-class-typep))
;;; ... and while it builds the rest (HOST-PCL-BUILD); until then, the
;;; cross-compiler's
(defun install-host-pcl-operators ()
  (setf (fdefinition 'sb-xc:ensure-generic-function) #'real-ensure-generic-function
        ;; (methods.lisp's and fngen.lisp's, which load after this file)
        (fdefinition 'add-to-weak-hashset) #'host-add-to-weak-hashset
        (fdefinition 'remove-from-weak-hashset) #'host-remove-from-weak-hashset
        (fdefinition 'weak-hashset-memberp) #'host-weak-hashset-memberp
        (fdefinition 'pcl-compile) #'host-pcl-compile
        (fdefinition '(setf slot-value)) #'host-set-slot-value
        (fdefinition 'class-has-a-forward-referenced-superclass-p)
        #'host-class-has-a-forward-referenced-superclass-p
        (fdefinition 'host-real-load-defmethod) #'load-defmethod
        (fdefinition 'load-defmethod) #'host-load-defmethod))

;;;; Target operators that the host lacks

(defun sb-c::%instance-typep (x layout)
  (let ((actual (and (host-%instancep x) (sb-kernel:layout-of x))))
    (and actual
         (or (eq actual layout)
             (and (typep actual 'sb-kernel:layout)
                  (find layout (sb-kernel:layout-inherits actual))))
         t)))

;;; target-defstruct.lisp's, as structure classes are finalized
;;; (std-class.lisp): its plain vector of slot names and their bits, which
;;; genesis writes into the structure's layout.  (The target makes a faster
;;; mapper for big structures; STRUCTURE-SLOT-VALUE takes either.)
(defun sb-kernel::install-struct-slot-mapper (layout)
  (let* ((slots (sb-kernel:dd-slots (sb-kernel:layout-dd layout)))
         (n (length slots))
         (vector (make-array (* n 2))))
    (loop for dsd in slots for i from 0
          do (setf (svref vector i) (sb-kernel:dsd-name dsd)
                   (svref vector (+ i n)) (sb-kernel::dsd-bits dsd)))
    (push (cons (sb-kernel:classoid-name (sb-kernel:layout-classoid layout)) vector)
          *host-layout-slot-mappers*)))

;;; (SETF SLOT-VALUE), as defsetfs.lisp's SET-SLOT-VALUE (PCL proper, which
;;; the host compiled, calls the function)
(defun host-set-slot-value (new-value object slot-name)
  (set-slot-value object slot-name new-value))

;;; std-class.lisp's, which the cross-compiler has too (a stub in
;;; cross-type.lisp): host PCL's own
(declaim (ftype function class-direct-superclasses)) ; host PCL defines it later
(defun host-class-has-a-forward-referenced-superclass-p (class)
  (or (when (forward-referenced-class-p class)
        class)
      (some #'class-has-a-forward-referenced-superclass-p
            (class-direct-superclasses class))))

;;; Dispatch functions and the like, generated while the host runs PCL, are
;;; the host's, and SB-XC:COMPILE makes the target's of the same code.
(defvar *host-pcl-compile-count* 0)
;;; SB-XC:COMPILE: called with what PCL compiles, it names the target's
;;; function of it (TEMPLATE-LAMBDA), (TEMPLATE :COLD-COMPILE n), and, if
;;; FORM is a generator (LAMBDA parameters ... #'(LAMBDA ...)), its closures'
;;; function, (TEMPLATE :COLD-COMPILE n :CLOSURE), which genesis
;;; cross-compiles into the cold core (COLD-COMPILE): those are what the
;;; dump refers to.
(defvar *host-pcl-cold-compiles* nil) ; (name . lambda), last first
(defun host-pcl-cold-compile (form)
  (let* ((name `(template :cold-compile ,(1+ (length *host-pcl-cold-compiles*))))
         (parameters nil)
         (closure-name nil))
    (when (eq (car form) 'sb-int:named-lambda)      ; its name is NAME
      (setq form `(lambda ,@(cddr form))))
    (aver (eq (car form) 'lambda))
    (setq parameters (second form))
    (let* ((body (cddr form))
           (inner (car (last body)))
           ;; the closure it makes: (FUNCTION (LAMBDA ...)), or (LAMBDA ...) or
           ;; (NAMED-LAMBDA ...) (fngen.lisp's)
           (lambda (if (typep inner '(cons (eql function) (cons cons null))) (second inner) inner)))
      (when (typep lambda '(cons (member lambda sb-int:named-lambda)))
        (setq closure-name (append name '(:closure)))
        (setq form `(lambda ,(second form)
                      ,@(butlast body)
                      (function (sb-int:named-lambda ,closure-name
                                  ,@(if (eq (car lambda) 'lambda)
                                        (cdr lambda)
                                        (cddr lambda))))))))
    (push (cons name form) *host-pcl-cold-compiles*)
    (values name closure-name parameters)))
(defun host-pcl-compile (expr safety)
  (declare (ignore safety))
  (incf *host-pcl-compile-count*)
  (let ((host (let ((*error-output* (make-broadcast-stream)))
                (handler-bind ((warning #'muffle-warning))
                  ;; (the host's COMPILE takes a LAMBDA only)
                  (if (typep expr '(cons (eql sb-int:named-lambda) (cons t list)))
                      (host-named (second expr) (compile nil `(lambda ,@(cddr expr))))
                      (values (compile nil expr)))))))
    (multiple-value-bind (name closure-name parameters)
        (host-pcl-cold-compile expr)
      (host-template name nil
                     (if closure-name
                         ;; the closures it makes are the target's closures
                         ;; of CLOSURE-NAME over the same values
                         (lambda (&rest arguments)
                           (let ((closure (apply host arguments)))
                             (host-template closure-name
                                            (mapcar #'cons parameters arguments)
                                            closure)))
                         host)))))

;;; TYPEP on PCL classes (their host types: cross-pcl.lisp).  The target's
;;; is a layout test; the host asks the class's precedence list, which the
;;; bootstrap fills in before any such test is made (see pre-warm.lisp's
;;; caveat on the class predicates).
(defun host-pcl-class-typep (x class-name)
  (let ((wrapper (and (host-pcl-instance-p x) (sb-kernel:layout-of x))))
    (and (host-wrapper-p wrapper)
         (let ((class (wrapper-class wrapper)))
           (and class
                (member class-name (early-class-precedence-list class)
                        :key (lambda (c)
                               (clos-slots-ref (get-slots c) (!bootstrap-slot-index 'class 'name))))
                t)))))

;;;; Loading target sources into the host

(defun host-pcl-canonical-symbol (symbol)
  ;; PCL's definitions of CL's names are SB-XC's, as genesis may have them CL's
  (if (and (eq (cl:symbol-package symbol) (find-package "CL"))
           (or (member (symbol-name symbol) sb-cold::*pcl-shadows* :test #'string=)
               (member (symbol-name symbol) sb-cold::*pcl-class-shadows* :test #'string=)))
      (find-symbol (symbol-name symbol) "SB-XC")
      symbol))

;;; Data from genesis (cold methods, their plists), whose CLOS names are CL's
(defun host-pcl-canonical (x)
  (cond ((symbolp x) (host-pcl-canonical-symbol x))
        ((consp x) (cons (host-pcl-canonical (car x)) (host-pcl-canonical (cdr x))))
        (t x)))

;;; *BUILT-IN-CLASSES* holds each built-in class's prototype, made by a
;;; target form (e.g. (SB-SYS:INT-SAP 0)) that the host cannot evaluate.  The
;;; host's has the cross-compiler's lattice (defs.lisp); CLASS-PROTOTYPE
;;; takes each prototype from the target's own *BUILT-IN-CLASSES*.
(defun host-pcl-built-in-classes ()
  (setq *built-in-classes*
        (mapcar (lambda (entry)
                  (destructuring-bind (name supers subs cpl prototype-form) entry
                    (declare (ignore prototype-form))
                    (list name supers subs cpl
                          (make-host-recipe '!built-in-class-prototype name))))
                *built-in-classes*)))

;;; boot.lisp's: the methods that host PCL makes, of which genesis gives those
;;; of the cross-compiled methods their plists (FIX-HOST-PCL-METHOD-PLISTS)
(defvar *host-pcl-methods* nil)
(defun host-load-defmethod (&rest args)
  (let ((method (apply #'host-real-load-defmethod args)))
    (push method *host-pcl-methods*)
    method))
;;; The plists of the cross-compiled methods, which genesis has (FOP-MSET):
;;; methods built on the host get these, as they go with the cross-compiled
;;; functions genesis gives them.  The host walks their bodies as host code
;;; (HOST-WALK-FORM), which tells nothing of their slot accesses: their
;;; :SLOT-NAME-LISTS, of which the method's pv table is made.
(defun fix-host-pcl-method-plists ()
  (dolist (method *host-pcl-methods*)
    (let* ((mf (!bootstrap-get-slot 'standard-method method '%function))
           (ff (and (%method-function-p mf) (%method-function-fast-function mf)))
           (name (and ff (gethash ff *host-fun-names*)))
           (record (and (typep name '(cons (eql fast-method)))
                        (gethash (cdr name) sb-fasl::*cold-method-records*))))
      (when record
        (let* ((plist (copy-list (host-pcl-canonical (getf (sixth record) 'plist))))
               (snl (getf plist :slot-name-lists)))
          (!bootstrap-set-slot 'standard-method method 'plist
                               (if snl
                                   (list* :pv-table (intern-pv-table :slot-name-lists snl) plist)
                                   plist)))))))


(defun host-pcl-target-only-stems ()
  (let ((stems nil))
    (sb-cold::do-stems-and-flags (stem flags 2)
      (when (member :pcl-target-only flags)
        (push stem stems)))
    (nreverse stems)))

;;; The classes of the files whose methods' bodies are the target's only
(defvar *target-only-class-names* nil)

;;; These (cache.lisp's etc., before boot.lisp defines DEFMETHOD) are the
;;; target's: they are cold methods, which warm load installs
;;; (!INSTALL-CROSS-COMPILED-METHODS).  The host keeps them for the record.

;;; Initfunctions and default-initarg functions are host closures made by the
;;; DEFCLASS forms of defs.lisp.  The target makes the same ones when its
;;; copy of defs.lisp runs, so each is identified by where it occurs.
;;; (Global functions, like CONSTANTLY-NIL, are the target's of the same name.)
(defun host-global-function-p (f)
  (let ((name (nth-value 2 (function-lambda-expression f))))
    (and name (symbolp name) (fboundp name) (eq (fdefinition name) f))))
(defun register-ecd-functions ()
  ;; Each is (LAMBDA () form), cross-compiled (SB-XC:COMPILE)
  (flet ((register (f form)
           (host-template (host-pcl-cold-compile `(lambda () ,form)) nil f)))
    (dolist (ecd *!early-class-definitions*)
      (dolist (slot (ecd-canonical-slots ecd))
        (let ((f (getf slot :initfunction)))
          (when (and (functionp f) (not (host-global-function-p f)))
            (register f (getf slot :initform)))))
      (loop for (key value) on (ecd-other-initargs ecd) by #'cddr
            when (eq key :direct-default-initargs)
              do (dolist (initarg value)
                   (when (and (functionp (third initarg))
                              (not (host-global-function-p (third initarg))))
                     (register (third initarg) (second initarg))))))))

(defvar *host-pcl-globals* nil) ; SB-PCL symbols whose values genesis dumps

(defun host-pcl-dumped-global-p (value)
  ;; holds PCL objects, and nothing the host made for itself only
  (let ((seen (make-hash-table :test 'eq)) (pcl nil) (host-only nil) (recipe nil))
    (labels ((walk (x)
               (unless (or (gethash x seen) (numberp x) (characterp x) (symbolp x) (stringp x))
                 (setf (gethash x seen) t)
                 (cond ((typep x '(cons (eql :mock-mutex)))) ; a new mutex in the core
                       ((consp x) (walk (car x)) (walk (cdr x)))
                       ((simple-vector-p x) (map nil #'walk x))
                       ((or (host-instance-p x) (host-funcallable-record x) (host-wrapper-p x))
                        (setq pcl t))
                       ((hash-table-p x) (maphash (lambda (k v) (walk k) (walk v)) x))
                       ;; the target's functions: cross-compiled (SB-XC:COMPILE)
                       ;; or templates, which genesis makes
                       ((and (functionp x) (gethash x *host-templates*))
                        (setq pcl t))
                       ((functionp x) (setq host-only t))
                       ;; PCL's own structures (CODEGEN-PARMS, ...), which
                       ;; genesis writes from their DDs
                       ((host-recipe-p x) (setq recipe t))
                       ((and (typep x 'structure-object)
                             (eq (cl:symbol-package (type-of x)) (find-package "SB-PCL")))
                        (setq pcl t))
                       ((typep x 'structure-object)
                        (if (host-recipe-p x) (setq recipe t) (setq host-only t)))))))
      (walk value))
    ;; or is data: PCL is bootstrapped on the host, so its state is the
    ;; host's, which genesis writes as it is, rather than what the target's
    ;; toplevel forms would compute again
    (and (or pcl (not recipe))
         (not host-only)
         ;; (an empty table is the target's toplevel form's to make)
         (not (and (hash-table-p value) (zerop (hash-table-count value)))))))

;;; PCL's macros of CLOS's names (SB-XC:DEFMACRO, boot.lisp's) are the host's:
;;; the cross-compiler's
(defun host-pcl-clos-macro (name)
  (let ((expander (sb-int:info :function :macro-function name)))
    (setf (cl:macro-function name) (lambda (form env) (funcall expander form env)))))
;;; ... while host PCL runs PCL's DEFMETHODs, DEFGENERICs and
;;; DEFINE-METHOD-COMBINATIONs: on the host otherwise they are host code's,
;;; or queue PCL proper's (cross-early.lisp)
(defun call-with-host-pcl-clos-macros (fun &aux (*host-pcl-evaluating* t))
  (let* ((names '(sb-xc:defmethod sb-xc:defgeneric sb-xc:define-method-combination))
         (saved (mapcar #'macro-function names)))
    (mapc #'host-pcl-clos-macro names)
    (unwind-protect (funcall fun)
      (loop for name in names for f in saved
            do (setf (macro-function name) f)))))
;;; SB-XC:GENSYM's symbols are all named alike (cross-misc.lisp), but genesis
;;; tells the variables that the closures of SB-XC:COMPILE's code close over
;;; by their names (FOP-TEMPLATE-CLOSURE): while host PCL makes code they are
;;; numbered, by a count of its own.
(defvar *host-gensym-count* 0)
(defun call-with-host-gensyms (fun)
  (let ((xc-gensym (fdefinition 'sb-xc:gensym)))
    (setf (fdefinition 'sb-xc:gensym)
          (lambda (&optional (thing "G"))
            (make-symbol (format nil "~A~D" thing (incf *host-gensym-count*)))))
    (unwind-protect (funcall fun)
      (setf (fdefinition 'sb-xc:gensym) xc-gensym))))
;;; The variables of SB-PCL bound before host PCL boots (class-init.lisp),
;;; but for those of the cross-compiler's copy of PCL's files, which are host
;;; PCL's: genesis dumps those of the others that hold PCL objects
(defvar *host-pcl-variables-before*) ; (symbol . value)
(defun host-pcl-variables ()
  (let (l)
    (dolist (package '("SB-PCL") l)
      (do-symbols (s package)
        (when (and (boundp s) (not (gethash s sb-cold::*pcl-host-definitions*)))
          (push (cons s (symbol-value s)) l))))))
;;; PCL proper's classes, generic functions and methods, as the cross-compiler
;;; is built (fixup.lisp, once PCL proper has defined them).  What the host
;;; compiles of them warns as host code: their methods' bodies are the
;;; cross-compiler's.
(defun host-pcl-build-proper ()
  (handler-bind ((warning #'muffle-warning))
    (cl:with-compilation-unit (:override t) ; (the host's)
      (host-pcl-build-proper-1))))
(defun host-pcl-build-proper-1 ()
  (call-with-host-gensyms
   (lambda (&aux ;; the cross-compiler's, as make-host-2 sets it, which PCL's
                 ;; code consults (REAL-ADD-METHOD ...)
                 (sb-c::*policy*
                  (sb-c::make-policy (loop for i below sb-c::n-policy-primary-qualities
                                           sum (ash #b01 (* i 2))))))
     (dolist (ecd (reverse *!early-class-definitions*)) ; (in defs.lisp's order)
       (note-dispatch-form
        :class
        `(defclass ,(ecd-class-name ecd) ,(ecd-superclass-names ecd)
           ,(loop for slot in (ecd-canonical-slots ecd)
                  collect `(,(getf slot :name)
                            ,@(loop for reader in (getf slot :readers) append `(:reader ,reader))
                            ,@(loop for writer in (getf slot :writers) append `(:writer ,writer)))))))
     (register-ecd-functions)
     ;; what loading PCL proper's files recorded
     (take-pass-2-forms)
     (setq sb-cold::*host-pcl-record* nil)
     (install-host-pcl-operators)
     (call-with-host-pcl-clos-macros #'run-pass-2-proper)
     (make-structure-classes-so-far))))

;;; The rest of PCL's bootstrap, in genesis, which writes it out
(defun host-pcl-build ()
  (call-with-host-gensyms #'host-pcl-build-1))
(defun host-pcl-build-1 ()
  (let ((before *host-pcl-variables-before*))
    (fix-host-pcl-method-plists)
    (call-with-host-pcl-clos-macros #'run-pass-2)
    ;; Dump the globals that hold PCL objects.  The rest (pure data, and the
    ;; host's own caches and compiled functions) the target computes itself.
    (dolist (package '("SB-PCL"))
     (do-symbols (s package)
      (when (and (member (cl:symbol-package s) (list (find-package "SB-PCL")))
                 (boundp s) (not (constantp s))
                 (not (assoc s before))
                 (host-pcl-dumped-global-p (symbol-value s)))
        (pushnew s *host-pcl-globals*))))
    (format t "~&; host PCL: ~D classes, ~D wrappers, ~D globals, ~D compiles~%"
            (length (host-pcl-classes)) (hash-table-count *host-wrappers*)
            (length *host-pcl-globals*) *host-pcl-compile-count*)))
