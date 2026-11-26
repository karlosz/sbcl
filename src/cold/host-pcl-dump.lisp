;;;; Writing what host PCL built (src/cold/host-pcl.lisp) into the cold core.
;;;;
;;;; This knows how to write the host's proxies as target objects, and
;;;; nothing about PCL: PCL objects come out the way PCL's own code, run on
;;;; the host, made them.  What only the target can make (a recipe) is an
;;;; error: such objects are made when first needed instead (lazy tables,
;;;; prototypes).
;;;;
;;;; As host PCL's build ends, HOST-PCL-WRITE-OBJECTS walks what it built
;;;; and writes it into the fasl of src/pcl/host-pcl-objects.lisp, as
;;;; FOP-HOST-PCL operations; genesis carries them out as it loads the file
;;;; (HOST-PCL-OP in genesis.lisp), once the object files are loaded, finding
;;;; what it has made of them already (layouts, classoids, method functions,
;;;; global functions) by name.

(in-package "SB-FASL")

;;; PCL's tables that its accessors make when first needed (DEFINE-LAZY-TABLE):
;;; NIL in the core, whatever host PCL put in them
(defparameter *host-pcl-lazy-tables*
  '(sb-pcl::*fgens* sb-pcl::*all-ctors* sb-pcl::**typecheck-cache**
    sb-pcl::*standard-slot-locations* sb-pcl::*pv-tables*
    sb-pcl::*eql-specializer-table* sb-pcl::*class-eq-specializer-methods*
    sb-pcl::**method-combinations**))
;;; ... of which genesis writes the entries (SB-PCL::!GENESIS-TABLE)
(defparameter *host-pcl-genesis-tables*
  '(sb-pcl::*eql-specializer-table* sb-pcl::*class-eq-specializer-methods*
    sb-pcl::**method-combinations**))
;;; Call one of host PCL's functions, by the target's name
(defmacro host-call (name &rest args)
  `(funcall ',name ,@args))



;;;; The writer, as host PCL's build ends: HP-DUMP-OBJECT, by which the
;;;; cross-compiler's dumper writes host PCL's objects as FOP-HOST-PCL
;;;; operations (SB-C::*XC-DUMP-OBJECT*).  An object is
;;;; made where the dumper first meets it, and in the fasl's table from then
;;;; on, and filled in later (*HP-FILLS*), so its graph's cycles are table
;;;; references; the dumper writes the rest (conses, vectors, constants,
;;;; template closures, types) as it writes any.


(defvar *hp-fasl*)                      ; the fasl-output
(defvar *hp-written*)                   ; the objects made, for the report
(defvar *hp-templates-written*)
(defvar *hp-pv-tables*)                 ; the pv tables written, last first
(defvar *hp-reset-copies*)              ; structures written for others

;;; Dump the operation OP (a keyword) on ARGS; its value is pushed, if PUSHP
(defun hp-op (pushp op &rest args)
  (let ((file *hp-fasl*))
    (sb-c::sub-dump-object op file)
    (dolist (arg args) (sb-c::sub-dump-object arg file))
    (if pushp
        (sb-c::dump-fop 'fop-host-pcl file (1+ (length args)))
        (sb-c::dump-fop 'fop-host-pcl-effect file (1+ (length args))))))
;;; X, made by OP on ARGS: in the fasl's table from now on, and filled in
;;; by FILL later; true, as HP-DUMP-OBJECT returns for what it dumps
(defun hp-alloc (x fill op &rest args)
  (apply #'hp-op t op args)
  (sb-c::eq-save-object x *hp-fasl*)
  (setf (gethash x *hp-written*) t)
  (when fill (push fill sb-c::*xc-dump-fills*))
  t)
;;; ... as is a function genesis has already, by its name or key, which is
;;; not counted.  (Names, which genesis looks things up by as it loads them,
;;; are copies: not shared with what may be written yet, which the dumper
;;; would patch in later.)
(defun hp-found (x op name)
  (hp-op t op (copy-tree name))
  (sb-c::eq-save-object x *hp-fasl*)
  t)

;;; Something genesis cannot write: an object only cold init could make
(defun host-pcl-unwritable (x &optional recipe)
  (let ((*print-level* 3) (*print-length* 5) (*print-circle* t))
    (error "host PCL: genesis cannot write ~S~@[ (class ~S)~] (~S)"
           (sb-pcl::dispatch-printable x)
           (and (sb-pcl::host-instance-p x)
                (ignore-errors (host-call class-name x)))
           recipe)))
;;; What the target makes itself, which no slot may hold
(defun host-pcl-recipe-of (x)
  (cond ((sb-pcl::host-recipe-p x) x)
        ((sb-pcl::host-weak-pointer-p x)
         (sb-pcl::make-host-recipe 'make-weak-pointer (sb-pcl::host-weak-pointer-value x)))
        ;; a lazy table instead (DEFINE-LAZY-TABLE, SB-PCL::!GENESIS-TABLE)
        ((hash-table-p x)
         (sb-pcl::make-host-recipe 'make-hash-table :test (hash-table-test x)))))


;;; A global function's name: the target's of the same name
(defun hp-global-function-name (x)
  (let ((name (nth-value 2 (function-lambda-expression x))))
    (when (and (typep name '(or symbol (cons (eql setf) (cons symbol null))))
               name (fboundp name) (eq (fdefinition name) x))
      name)))

;;; Slots that hold the host's state that the target makes again: method
;;; combinations' caches of effective method functions.  (Discriminating
;;; functions and their caches are the target's.)
(defparameter *host-pcl-reset-slots* '((sb-pcl::%cache . nil)))
(defparameter *host-pcl-reset-struct-slots*
  '(sb-pcl::gf-info-static-c-a-m-emf sb-pcl::gf-info-cnm-checker))
(defvar *host-pcl-slot-names*) ; wrapper -> vector of slot names by location
(defun host-pcl-slot-names (wrapper)
  (or (gethash wrapper *host-pcl-slot-names*)
      (setf (gethash wrapper *host-pcl-slot-names*)
            (let* ((class (sb-kernel:classoid-pcl-class (sb-kernel:layout-classoid wrapper)))
                   (names (make-array (sb-kernel:layout-length wrapper) :initial-element nil)))
              (when class
                (dolist (slotd (host-call sb-mop:class-slots class))
                  (let ((location (host-call sb-mop:slot-definition-location slotd)))
                    (when (integerp location)
                      (setf (svref names location) (host-call sb-mop:slot-definition-name slotd))))))
              names))))
(defun host-pcl-dumped-slots (wrapper slots)
  (let ((names (host-pcl-slot-names wrapper))
        (copy (copy-seq slots)))
    (dotimes (i (length copy) copy)
      (let* ((name (and (< i (length names)) (svref names i)))
             (reset (assoc name *host-pcl-reset-slots*)))
        (cond (reset (setf (svref copy i) (cdr reset)))
              ;; a built-in class's prototype, which CLASS-PROTOTYPE finds
              ;; in the target's *BUILT-IN-CLASSES*
              ;; (A structure or condition class's is NIL: CLASS-PROTOTYPE
              ;; makes it.)
              ((and (eq name 'sb-pcl::prototype)
                    (sb-pcl::host-recipe-p (svref copy i)))
               (setf (svref copy i)
                     (if (eq (sb-pcl::host-recipe-function (svref copy i))
                             'sb-pcl::!built-in-class-prototype)
                         (sb-kernel::make-unbound-marker)
                         nil))))))))

;;; X, if it is one of host PCL's objects (SB-C::*XC-DUMP-OBJECT*), made by
;;; its operation: true, or false for what the dumper writes as it writes
;;; any
(defun hp-dump-object (x file)
  (typecase x
    ;; a mutex the cross-compiler's MAKE-MUTEX made (cross-misc.lisp)
    ((cons (eql :mock-mutex)) (hp-alloc x nil :mutex (second x)))
    (function
     ;; a method's function: the cross-compiled one, by its name
     ;; (FOP-PCL-FUNCTION, SB-PCL::MAKE-METHODS)
     (let ((key (gethash x sb-pcl::*early-method-function-keys*)))
       (cond (key (hp-found x :method-function key))
             ;; a closure of a TEMPLATE-LAMBDA (src/cold/host-pcl.lisp): genesis
             ;; makes the target's closure of the template's function
             ;; (FOP-CLOSURE), over the values of its variables
             ((gethash x sb-pcl::*host-templates*)
              (destructuring-bind (template . values) (gethash x sb-pcl::*host-templates*)
                (when (eq (second template) :cold-compile) (incf *hp-templates-written*))
                ;; (names, as HP-FOUND's)
                (sb-c::sub-dump-object (copy-tree template) file)
                (sb-c::sub-dump-object (copy-tree (gethash x sb-pcl::*host-fun-names*)) file)
                (loop for (var . value) in values
                      do (sb-c::sub-dump-object var file)
                         (sb-c::sub-dump-object value file))
                (sb-c::dump-fop 'fop-closure file (length values))
                (sb-c::eq-save-object x file)
                t))
             ((sb-pcl::host-funcallable-record x) (hp-funcallable x))
             ((hp-global-function-name x)
              (hp-found x :global-function (hp-global-function-name x)))
             (t (error "host PCL: no target counterpart for ~S~@[ named ~S~]" x
                       (gethash x sb-pcl::*host-fun-names*))))))
    ;; a standard instance, which the dumper writes as a structure (its
    ;; layout and slot vector), with its slots as they are written (host
    ;; PCL's build is over)
    (sb-pcl::host-instance
     (let ((slots (sb-pcl::host-instance-slots x)))
       (replace slots (host-pcl-dumped-slots (sb-pcl::host-instance-wrapper x) slots)))
     (setf (gethash x (sb-c::fasl-output-valid-structures file)) t
           (gethash x *hp-written*) t)
     (sb-c::dump-structure x file)
     (sb-c::eq-save-object x file)
     t)
    ;; a layout: one of PCL's classes (a wrapper, its slots as host PCL set
    ;; them), or one of the cross-compiler's, which genesis may have
    (sb-kernel:layout
     (let ((pcl (sb-pcl::host-wrapper-p x)))
       (apply #'hp-alloc x
              (lambda ()
                (hp-op nil :layout-fill x pcl
                       (and pcl (sb-kernel:layout-classoid x))
                       (and pcl (sb-kernel::layout-%info x))
                       (sb-kernel::layout-slot-table x)))
              :layout (sb-kernel::layout-classoid-name x) pcl
              (sb-kernel:layout-depthoid x) (sb-kernel:layout-flags x)
              (sb-kernel:layout-length x) (sb-kernel:layout-bitmap x)
              ;; A class's current layout is the one of its name that
              ;; compiled code refers to (TYPEP)
              (and pcl (eq x (sb-kernel:classoid-layout (sb-kernel:layout-classoid x))))
              (typep (sb-kernel:layout-classoid x) 'sb-kernel:built-in-classoid)
              ;; (its ancestors', made first)
              (coerce (sb-kernel:layout-inherits x) 'list))))
    (sb-kernel:classoid (hp-dump-classoid x))
    ;; a method combination's generic functions (METHOD-COMBINATION-GF-SET)
    (sb-pcl::host-gf-hashset
     (sb-c::sub-dump-object (sb-pcl::host-gf-hashset-members x) file)
     t)
    ;; the dumper's
    (sb-kernel:ctype nil)
    ;; a host instance of a target defstruct (SB-PCL::SLOT-INFO ...), which
    ;; the dumper writes (DUMP-STRUCTURE), with the slots that hold the
    ;; host's state reset, but for what the target makes itself
    (t
     (let ((recipe (host-pcl-recipe-of x)))
       (cond (recipe (host-pcl-unwritable x recipe))
             ((and (typep x 'structure-object)
                   (sb-kernel:find-defstruct-description (type-of x) nil))
              (when (typep x 'sb-pcl::pv-table) ; the target interns it too
                (push x *hp-pv-tables*))
              (let ((dumped (if (gethash x *hp-reset-copies*) x (hp-reset-struct-slots x))))
                (setf (gethash dumped (sb-c::fasl-output-valid-structures file)) t)
                (unless (eq dumped x)
                  (setf (gethash dumped *hp-reset-copies*) t)
                  (sb-c::sub-dump-object dumped file)
                  (setf (gethash x (sb-c::fasl-output-eq-table file))
                        (gethash dumped (sb-c::fasl-output-eq-table file)))
                  t))))))))
;;; X, or a copy of it with its slots that hold the host's state reset
(defun hp-reset-struct-slots (x)
  (let ((resets (loop for dsd in (sb-kernel:dd-slots
                                  (sb-kernel:find-defstruct-description (type-of x)))
                      when (member (sb-kernel:dsd-accessor-name dsd) *host-pcl-reset-struct-slots*)
                        collect (sb-kernel:dsd-accessor-name dsd))))
    (if resets
        (let ((copy (copy-structure x)))
          (dolist (accessor resets copy)
            (funcall (fdefinition `(setf ,accessor)) nil copy)))
        x)))
;;; a funcallable structure (%METHOD-FUNCTION: its fast function is its one
;;; slot), or instance
(defun hp-funcallable (x)
  (let ((record (sb-pcl::host-funcallable-record x)))
    (if (typep record 'sb-pcl::host-%method-function-record)
        (hp-alloc x (lambda ()
                      (hp-op nil :%method-function-fill x
                             (sb-pcl::host-funcallable-function record)
                             (sb-pcl::host-%method-function-record-fast-function record)))
                  :%method-function)
        (hp-alloc x (lambda ()
                      (hp-op nil :fin-fill x
                             ;; its discriminating function, as host PCL left it
                             (or (sb-pcl::host-funcallable-function record)
                                 (sb-pcl::host-fin-target-function record))
                             (sb-pcl::host-fin-wrapper record)
                             (host-pcl-dumped-slots (sb-pcl::host-fin-wrapper record)
                                                    (sb-pcl::host-fin-slots record))))
                  :fin (sb-pcl::host-fin-hash record)))))
(defun hp-dump-classoid (x)
  (let ((name (classoid-name x))
        (own (sb-pcl::host-own-classoid-p x)))
    (hp-alloc x (lambda ()
                  ;; as CTYPE-TO-CORE writes it, with its class and, if
                  ;; PCL's, the layout REGISTER-LAYOUT gave it on the host
                  (let ((dd (sb-kernel:find-defstruct-description (type-of x))))
                    (flet ((index (slot)
                             (sb-kernel:dsd-index (find slot (sb-kernel:dd-slots dd)
                                                        :key #'sb-kernel:dsd-name))))
                      (sb-c::dump-structure-fills
                       x *hp-fasl*
                       (lambda (index)
                         (cond ((= index (index 'sb-kernel::subclasses)) nil)
                               ((= index (index 'sb-kernel::layout))
                                (and own (sb-kernel:classoid-layout x)))
                               ;; (patched in in cold init)
                               ((and (typep x 'sb-kernel:built-in-classoid)
                                     (= index (index 'sb-kernel::predicate)))
                                (sb-kernel::make-unbound-marker))
                               (t (sb-kernel:%instance-ref x index)))))))
                  (hp-op nil :classoid-fill x (type-of x) name own))
              :classoid name (type-of x) (eq (sb-kernel:find-classoid name nil) x))))

;;; The classoids of PCL's classes and layouts: its own, and the
;;; cross-compiler's of the classes it made of them
(defun host-pcl-classoids ()
  (let ((seen (make-hash-table :test 'eq)) (result nil))
    (flet ((note (classoid)
             (unless (gethash classoid seen)
               (setf (gethash classoid seen) t)
               (push classoid result))))
      (maphash (lambda (layout v) (declare (ignore v)) (note (sb-kernel:layout-classoid layout)))
               sb-pcl::*host-pcl-layouts*)
      (loop for (nil . class) in (sb-pcl::host-pcl-classes)
            do (let ((wrapper (ignore-errors (host-call sb-pcl::class-wrapper class))))
                 (when (typep wrapper 'sb-kernel:layout)
                   (note (sb-kernel:layout-classoid wrapper))))))
    (nreverse result)))

;;; What PCL's toplevel forms installed on the host as global functions:
;;; generic functions by their names, and functions installed under other
;;; names (LOAD-DEFCLASS is REAL-LOAD-DEFCLASS, ...): (name . function)s,
;;; and the generic functions
(defun host-pcl-fdefinitions ()
  (let ((gfs nil) (fsets nil))
    (maphash (lambda (fin record)
               (when (and (sb-pcl::host-fin-p record) (host-call sb-pcl::generic-function-p fin))
                 (if (host-call sb-pcl::standard-generic-function-p fin)
                     (push fin gfs)
                     (format t "~&; host PCL: not a standard generic function: ~S~%"
                             (sb-xc:class-name (sb-xc:class-of fin))))
                 (when (and (host-call sb-pcl::standard-generic-function-p fin) ; not anonymous
                            (sb-xc:slot-boundp fin 'sb-pcl::name))
                   (push (cons (host-call sb-mop:generic-function-name fin) fin) fsets))))
             sb-pcl::*host-funcallables*)
    (setq fsets (nreverse fsets))
    (flet ((alias (name)
             (when (and (fboundp name) (not (macro-function (if (consp name) (second name) name))))
               (let* ((f (fdefinition name))
                      (other (or (gethash f sb-pcl::*host-fun-names*)
                                 (nth-value 2 (function-lambda-expression f)))))
                 (when (and (not (sb-pcl::host-funcallable-record f))
                            (typep other '(or symbol (cons (eql setf) (cons symbol null))))
                            other (not (equal other name))
                            (fboundp other) (eq (fdefinition other) f)
                            (not (search "HOST" (string (if (consp other) (second other) other)))))
                   (setq fsets (nconc fsets (list (cons name f)))))))))
      ;; host PCL's own names, and its shadows of other packages'
      (do-symbols (s "SB-PCL")
        (when (and (eq (cl:symbol-package s) (find-package "SB-PCL"))
                   (not (search "HOST" (symbol-name s))))
          (alias s)
          (alias `(setf ,s)))))
    (values fsets gfs)))

;;; src/pcl/host-pcl-objects.lisp's forms: the functions host PCL compiled
;;; (a list of them, so that they are compiled)
(sb-xc:defmacro sb-pcl::!host-pcl-compiled-functions ()
  `(list ,@(loop for (name . form) in (reverse sb-pcl::*host-pcl-cold-compiles*)
                 collect `#'(named-lambda ,name ,@(cdr form)))))
;;; ... and the constant which they are, as the dumper writes it
(sb-xc:defmacro sb-pcl::!host-pcl-objects ()
  `',(sb-c::make-deferred-dump #'host-pcl-dump-objects))
(defun host-pcl-dump-objects (file)
  (let ((*hp-fasl* file)
        (sb-c::*xc-dump-object* #'hp-dump-object)
        (*hp-templates-written* 0)
        (*hp-pv-tables* nil)
        (*hp-reset-copies* (make-hash-table :test 'eq)))
    (host-pcl-write-1))
  (sb-c::sub-dump-object nil file))
(defun host-pcl-write-1 ()
  (let ((sb-c::*xc-dump-fills* nil)
        (*hp-written* (make-hash-table :test 'eq))
        (*host-pcl-slot-names* (make-hash-table :test 'eq))
        (classoids (host-pcl-classoids)))
    (labels ((fill-all () (sb-c::dump-xc-fills))
             ;; (with what its arguments lead to written first, as genesis
             ;; may look for it)
             (op (&rest args) (fill-all) (apply #'hp-op nil args))
             ;; (made, and in the table)
             (dump (x)
               (sb-c::sub-dump-object x *hp-fasl*)
               (sb-c::dump-pop *hp-fasl*)))
      ;; FIND-CLASS: the class in its name's classoid cell, as (SETF FIND-CLASS)
      ;; puts it before the boot state is COMPLETE
      (loop for (name . class) in (sb-pcl::host-pcl-classes)
            do (op :classoid-cell-class name class))
      (dolist (classoid classoids)
        (when (sb-kernel:classoid-pcl-class classoid)
          (if (or (sb-pcl::host-own-classoid-p classoid)
                  (typep classoid '(or sb-kernel:built-in-classoid
                                    sb-kernel:structure-classoid
                                    sb-kernel::condition-classoid sb-kernel::static-classoid)))
              (dump classoid)
              ;; (unless genesis has made it)
              (op :xc-classoid-required (classoid-name classoid) classoid))))
      ;; slot tables of layouts that are not PCL's (STRUCTURE-OBJECT etc.)
      (dolist (classoid classoids)
        (let* ((xc-layout (sb-kernel:classoid-layout classoid))
               (table (and xc-layout (not (sb-pcl::host-wrapper-p xc-layout))
                           (sb-kernel::layout-slot-table xc-layout))))
          (when table
            (op :layout-slot (classoid-name classoid) 'sb-kernel::slot-table table))))
      ;; ... and their slot mappers
      (loop for (name . mapper) in sb-pcl::*host-layout-slot-mappers*
            do (op :layout-slot name 'sb-kernel::slot-mapper mapper))
      (dolist (classoid classoids)
        (when (and (sb-pcl::host-own-classoid-p classoid)
                   (eq (sb-kernel:find-classoid (classoid-name classoid) nil) classoid))
          (dump classoid)))       ; filed in its cell
      ;; Each classoid PCL made is a subclassoid of its layout's ancestors'
      ;; classoids, as REGISTER-LAYOUT makes it (ADD-SUBCLASSOID's alist: the
      ;; target makes a hash table of a long one when it adds to it).
      (let ((subclassoids (make-hash-table :test 'eq)) ; super -> ((sub . layout) ...)
            (supers nil))
        (dolist (classoid classoids)
          (let ((layout (and (sb-pcl::host-own-classoid-p classoid)
                             (sb-kernel:classoid-layout classoid))))
            (when layout
              (loop for super across (sb-kernel:layout-inherits layout)
                    do (let ((super (sb-kernel:layout-classoid super)))
                         (unless (gethash super subclassoids) (push super supers))
                         (push (cons classoid layout) (gethash super subclassoids)))))))
        (dolist (super (nreverse supers))
          (apply #'op :subclassoids super
                 (loop for (sub . layout) in (gethash super subclassoids)
                       collect sub collect layout))))
      ;; global functions and variables
      (multiple-value-bind (fsets gfs) (host-pcl-fdefinitions)
        (loop for (name . function) in fsets
              do (fill-all)
                 (sb-c::sub-dump-object (copy-tree name) *hp-fasl*)
                 (sb-c::sub-dump-object function *hp-fasl*)
                 (sb-c::dump-fop 'fop-fset *hp-fasl*))
        (dolist (symbol (remove-if (lambda (s) (member s *host-pcl-lazy-tables*))
                                   sb-pcl::*host-pcl-globals*))
          (let ((value (symbol-value symbol)))
            (if (host-pcl-recipe-of value)
                (host-pcl-unwritable value) ; a hash table, say: a lazy table instead
                (op :cold-set symbol value))))
        (op :cold-set 'sb-pcl::*!braid-dumped* t)
        (dolist (symbol *host-pcl-lazy-tables*) (op :cold-set symbol nil))
        ;; the function generators host PCL made, which SB-XC:COMPILE compiled
        (op :cold-set 'sb-pcl::*!genesis-fgens*
            (let ((fgens (symbol-value (find-symbol "*FGENS*" "SB-PCL")))
                  (entries nil))
              (when fgens
                (maphash (lambda (test fgen) (push (cons test fgen) entries)) fgens))
              entries))
        (op :cold-set 'sb-pcl::*!genesis-table-entries*
            (loop for name in *host-pcl-genesis-tables*
                  for table = (symbol-value name)
                  when table
                    collect name
                    and collect (let (entries)
                                  (maphash (lambda (k v) (push v entries) (push k entries))
                                           table)
                                  entries)))
        (fill-all)
        ;; the pv tables the objects led to
        (op :cold-set 'sb-pcl::*!genesis-pv-tables* *hp-pv-tables*)
        (op :report (hash-table-count *hp-written*) (length gfs)
            (length sb-pcl::*host-pcl-cold-compiles*) *hp-templates-written*)
        ;; what was made, by type, if the build directory has a file host-pcl-histogram
        (when (probe-file "host-pcl-histogram")
          (let ((h (make-hash-table :test 'equal)))
            (maphash (lambda (k v) (declare (ignore v))
                       (incf (gethash (let ((ty (type-of k))) (if (consp ty) (car ty) ty)) h 0)))
                     *hp-written*)
            (maphash (lambda (k v) (format t "~&HIST ~S ~D~%" k v)) h)
            (maphash (lambda (k v) (declare (ignore v)) (when (symbolp k) (format t "~&SYM ~S~%" k)))
                     *hp-written*)))))))
