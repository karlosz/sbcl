;;;; Writing what host PCL built (src/cold/host-pcl.lisp) into the cold core.
;;;;
;;;; This knows how to write the host's proxies as target objects, and
;;;; nothing about PCL: PCL objects come out the way PCL's own code, run on
;;;; the host, made them.  What only the target can make (a recipe) is an
;;;; error: such objects are made when first needed instead (lazy tables,
;;;; prototypes).

(in-package "SB-FASL")

(defvar *host-pcl-memo*)
(defvar *host-pcl-pv-tables*)    ; the cold ones
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
(defvar *cold-compile-count* 0)   ; COLD-COMPILE's, below
;;; Call one of host PCL's functions, by the target's name
(defmacro host-call (name &rest args)
  `(funcall ',name ,@args))

(defun host-pcl-unbound-p (x) (eq x (sb-kernel::make-unbound-marker)))


;;; The cross-compiled function of a method, by its name
(defun host-pcl-cold-function (x)
  (let ((key (gethash x sb-pcl::*early-method-function-keys*))
        (name (gethash x sb-pcl::*host-fun-names*)))
    (cond (key (gethash key *cold-pcl-method-functions*))
          ((and (consp name) (eq (car name) 'sb-pcl::fast-method))
           (or (gethash (cdr name) *cold-pcl-method-functions*)
               (host-pcl-eql-method-function (cdr name)))))))

;;; ... of a method EQL-specialized: the host names its function by the
;;; object, and the cross-compiler by the form (*STANDARD-METHOD-COMBINATION*)
(defvar *host-pcl-eql-method-functions*)
(defun host-pcl-eql-method-function (name) ; (gf-name qualifier* specializers)
  (unless (boundp '*host-pcl-eql-method-functions*)
    (setq *host-pcl-eql-method-functions* nil)
    (maphash (lambda (key function)
               (let ((specializers (car (last key))))
                 (when (some (lambda (s) (typep s '(cons (eql eql)))) specializers)
                   (push (cons (append (butlast key)
                                       (list (mapcar (lambda (s)
                                                       (if (typep s '(cons (eql eql)))
                                                           `(eql ,(eval (sb-pcl::host-pcl-canonical (second s))))
                                                           s))
                                                     specializers)))
                               function)
                         *host-pcl-eql-method-functions*))))
             *cold-pcl-method-functions*))
  (let ((name (cons (car name) ; but the objects as they are
                    (mapcar (lambda (x)
                              (if (listp x)
                                  (mapcar (lambda (s)
                                            (if (typep s '(cons (eql eql))) s s))
                                          x)
                                  x))
                            (cdr name)))))
    (cdr (assoc name *host-pcl-eql-method-functions* :test #'equal))))

;;; A closure of a TEMPLATE-LAMBDA (low.lisp): the cross-compiler recorded
;;; which of the template's variables are in which of its closure's slots
;;; (FOP-TEMPLATE-CLOSURE).
(defvar *host-pcl-cold-compiled-written* 0)
(defun host-pcl-template-closure (x)
  (destructuring-bind (template . values) (gethash x sb-pcl::*host-templates*)
    (when (eq (second template) :cold-compile) (incf *host-pcl-cold-compiled-written*))
    (destructuring-bind (fun . vars) (cold-template-fun template)
      (when (eq vars :none)             ; it closes over nothing
        (return-from host-pcl-template-closure fun))
      (let* ((name (gethash x sb-pcl::*host-fun-names*))
             (closure (allocate-template-closure fun (length vars) name)))
        (setf (gethash x *host-pcl-memo*) closure)
        (loop for var in vars for i from 0
              do (write-wordindexed closure (+ sb-vm:closure-info-offset i)
                                    (host-pcl-slot-value (template-var-value template var values)
                                                         closure :closure i)))
        (when name
          (set-template-closure-name closure (length vars) (host-pcl-to-core name)))
        closure))))
;;; for the dumper (DUMP-TEMPLATE-CLOSURE)
(setq sb-c::*template-closure-of*
      (lambda (x)
        (destructuring-bind (template . values) (gethash x sb-pcl::*host-templates*)
          (values template values (gethash x sb-pcl::*host-fun-names*)))))

(defun host-pcl-recipe-of (x)
  (cond ((sb-pcl::host-recipe-p x) x)
        ((gethash x sb-pcl::*host-templates*) nil)
        ;; Layouts of structure and condition classoids that genesis has not
        ;; made: their DEFSTRUCTs make them in cold init.  (Built-in ones
        ;; genesis makes, below.)
        ((and (typep x 'sb-kernel:layout)
              (not (sb-pcl::host-wrapper-p x)) ; PCL's: written, below
              (not (gethash (classoid-name (sb-kernel:layout-classoid x)) *cold-layouts*))
              (not (typep (sb-kernel:layout-classoid x) 'sb-kernel:built-in-classoid)))
         (sb-pcl::make-host-recipe 'sb-pcl::!classoid-layout
                                   (classoid-name (sb-kernel:layout-classoid x))))
        ;; ... and their classoids
        ((and (typep x 'sb-kernel:classoid) (not (sb-pcl::host-own-classoid-p x))
              (not (host-pcl-cold-classoid (classoid-name x)))
              (not (typep x 'sb-kernel:built-in-classoid)))
         (sb-pcl::make-host-recipe 'find-classoid (classoid-name x)))
        ((sb-pcl::host-weak-pointer-p x)
         (sb-pcl::make-host-recipe 'make-weak-pointer (sb-pcl::host-weak-pointer-value x)))
        ;; a lazy table instead (DEFINE-LAZY-TABLE, SB-PCL::!GENESIS-TABLE)
        ((hash-table-p x)
         (sb-pcl::make-host-recipe 'make-hash-table :test (hash-table-test x)))
        ((and (functionp x) (not (sb-pcl::host-funcallable-record x))
              (not (host-pcl-cold-function x))
              (not (host-pcl-global-function x)))
         (error "host PCL: no target counterpart for ~S~@[ named ~S~]" x
                (gethash x sb-pcl::*host-fun-names*)))))

;;; A global function: the target's of the same name, which cold load defined
(defun host-pcl-global-function (x)
  (let ((name (nth-value 2 (function-lambda-expression x))))
    (when (and (typep name '(or symbol (cons (eql setf) (cons symbol null))))
               name (fboundp name) (eq (fdefinition name) x))
      (let* ((name name)
             (f (if (symbolp name)
                    (cold-symbol-function name nil)
                    (cold-fdefn-fun (ensure-cold-fdefn (host-pcl-to-core name))))))
        (unless (and f (not (cold-null f)))
          (error "host PCL: ~S is not defined in the cold core" name))
        f))))

;;; Something genesis cannot write: an object only cold init could make
(defun host-pcl-unwritable (x &optional (recipe (host-pcl-recipe-of x)))
  (let ((*print-level* 3) (*print-length* 5) (*print-circle* t))
    (error "host PCL: genesis cannot write ~S~@[ (class ~S)~] (~S)"
           (sb-pcl::dispatch-printable x)
           (and (sb-pcl::host-instance-p x)
                (ignore-errors (host-call class-name x)))
           recipe)))

;;; Convert X for storing at INDEX of CONTAINER (of KIND)
(defun host-pcl-slot-value (x container kind index)
  (declare (ignore container kind index))
  (if (host-pcl-recipe-of x)
      (host-pcl-unwritable x)
      (host-pcl-to-core x)))

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

(defun host-pcl-cold-classoid (name) ; the target's classoid, if genesis made it
  (let ((cell (gethash name *classoid-cells*)))
    (when cell
      (let ((classoid (read-slot cell :classoid)))
        (unless (cold-null classoid) classoid)))))

(defun host-pcl-classoid-to-core (classoid)
  ;; A classoid PCL made, or one of the cross-compiler's, whose copy the core
  ;; may have already, or for a built-in classoid will have
  ;; (!CLASSES-COLD-INIT finds it).  CTYPE-TO-CORE files it in its cell.
  (let* ((own (sb-pcl::host-own-classoid-p classoid))
         (pcl-class (sb-kernel:classoid-pcl-class classoid))
         (result (or (and (not own) (host-pcl-cold-classoid (classoid-name classoid)))
                     (progn
                       (when own        ; the target's symbol
                         (setf (classoid-name classoid)
                               (classoid-name classoid)))
                       (ctype-to-core classoid))))
         (layout (sb-kernel:classoid-layout classoid)))
    (when pcl-class (write-slots result :pcl-class (host-pcl-to-core pcl-class)))
    (cond ((and own layout)
           ;; the layout REGISTER-LAYOUT gave it on the host
           (write-slots result :layout (host-pcl-to-core layout)))
          ((typep classoid 'sb-kernel:structure-classoid)
           ;; its layout's classoid, which %DEFSTRUCT sets when its toplevel
           ;; form runs: PCL's code may meet its instances before (TYPE-OF of
           ;; a discriminating function's info, say)
           (let ((cold (gethash (classoid-name classoid) *cold-layouts*)))
             (when cold (write-slots (cold-layout-descriptor cold) :classoid result)))))
    result))

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
              ;; DEFGENERIC's :METHODs, as values of DEFMETHOD: during the
              ;; braid that is the early ADD-METHOD's, a discriminating function
              ((and (eq name 'sb-pcl::initial-methods) (listp (svref copy i)))
               (setf (svref copy i) (remove-if-not #'sb-pcl::host-instance-p (svref copy i))))
              ((eq name 'sb-pcl::%lock)
               (setf (svref copy i) (make-host-pcl-mutex :name "GF lock")))
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

;;; A mutex for the target: a new one, as MAKE-MUTEX makes (its raw words 0)
(defstruct (host-pcl-mutex (:constructor make-host-pcl-mutex (&key name))) name)

(defun host-pcl-to-core (x)
  (cond
    ((host-pcl-unbound-p x) (make-random-descriptor sb-vm:unbound-marker-widetag))
    ((descriptor-p x) x)                ; the core's already (an early method's source)
    ;; uninterned: each its own (genesis coalesces file constants by name,
    ;; but FNGEN's variables are all named G)
    ((and (symbolp x) (not (cl:symbol-package x)))
     (or (gethash x *host-pcl-memo*)
         (setf (gethash x *host-pcl-memo*)
               (setf (gethash (list :host-pcl (hash-table-count *uninterned-symbol-table*))
                              *uninterned-symbol-table*)
                     (allocate-symbol sb-vm:symbol-size nil (symbol-name x))))))
    ((symbolp x) (host-constant-to-core x))
    ((or (numberp x) (characterp x) (stringp x)
         ;; literal data of structure slots' defaults
         (bit-vector-p x)
         (and (typep x '(simple-array * (*))) (not (simple-vector-p x))))
     (host-constant-to-core x))
    ((and (functionp x) (host-pcl-cold-function x)))
    ((gethash x *host-pcl-memo*))
    ((gethash x sb-pcl::*host-templates*) (host-pcl-template-closure x))
    ((and (functionp x) (not (sb-pcl::host-funcallable-record x)) (host-pcl-global-function x)))
    ;; a mutex the cross-compiler's MAKE-MUTEX made (cross-misc.lisp)
    ((typep x '(cons (eql :mock-mutex)))
     (let ((mutex (allocate-struct-of-type 'sb-thread:mutex)))
       (setf (gethash x *host-pcl-memo*) mutex)
       (write-slots mutex :name (host-pcl-to-core (second x)))))
    ((consp x)
     (let ((head (cold-cons *nil-descriptor* *nil-descriptor*)))
       (setf (gethash x *host-pcl-memo*) head)
       (write-wordindexed head sb-vm:cons-car-slot (host-pcl-slot-value (car x) head :car 0))
       (write-wordindexed head sb-vm:cons-cdr-slot (host-pcl-slot-value (cdr x) head :cdr 0))
       head))
    ((simple-vector-p x)
     (let ((v (allocate-vector sb-vm:simple-vector-widetag (length x) (length x))))
       (setf (gethash x *host-pcl-memo*) v)
       (dotimes (i (length x) v)
         (write-wordindexed v (+ sb-vm:vector-data-offset i)
                            (host-pcl-slot-value (svref x i) v :svref i)))))
    ((sb-pcl::host-instance-p x)
     ;; Memoize before converting anything reachable: the wrapper leads back
     ;; through its class to the rest of the graph, possibly to X itself.
     (let* ((placeholder (cold-layout-descriptor (gethash 't *cold-layouts*)))
            (instance (allocate-struct (1+ sb-vm:instance-data-start) placeholder))
            (wrapper (sb-pcl::host-instance-wrapper x))
            (recipe (host-pcl-recipe-of wrapper)))
       (setf (gethash x *host-pcl-memo*) instance)
       (if recipe
           (host-pcl-unwritable wrapper recipe)
           (set-instance-layout instance (host-pcl-to-core wrapper)))
       (write-wordindexed instance (+ sb-vm:instance-slots-offset sb-vm:instance-data-start)
                          (host-pcl-to-core (host-pcl-dumped-slots
                                             (sb-pcl::host-instance-wrapper x)
                                             (sb-pcl::host-instance-slots x))))
       instance))
    ((typep (sb-pcl::host-funcallable-record x) 'sb-pcl::host-%method-function-record)
     ;; a funcallable structure: the fast function is its one slot
     (let* ((record (sb-pcl::host-funcallable-record x))
            (n-words (+ sb-vm:funcallable-instance-info-offset 1))
            (mf (allocate-object *dynamic* n-words sb-vm:fun-pointer-lowtag)))
       (setf (gethash x *host-pcl-memo*) mf)
       (write-header-word mf (sb-vm::compute-object-header
                              n-words sb-vm:funcallable-instance-widetag))
       (write-wordindexed/raw mf sb-vm:funcallable-instance-trampoline-slot
                              (lookup-assembler-reference 'sb-vm::funcallable-instance-tramp))
       (write-wordindexed mf sb-vm::funcallable-instance-function-slot
                          (host-pcl-slot-value (sb-pcl::host-funcallable-function record)
                                               mf :fin-function 0))
       (write-wordindexed mf sb-vm::funcallable-instance-layout-slot
                          (cold-layout-descriptor (gethash 'sb-pcl::%method-function *cold-layouts*)))
       (write-wordindexed mf sb-vm:funcallable-instance-info-offset
                          (host-pcl-slot-value
                           (sb-pcl::host-%method-function-record-fast-function record)
                           mf :fin-info 0))
       mf))
    ((sb-pcl::host-fin-record x)
     (let* ((record (sb-pcl::host-fin-record x))
            (n-words (+ sb-vm:funcallable-instance-info-offset 2))
            (fin (allocate-object *dynamic* n-words sb-vm:fun-pointer-lowtag)))
       (setf (gethash x *host-pcl-memo*) fin)
       (write-header-word fin (sb-vm::compute-object-header
                               n-words sb-vm:funcallable-instance-widetag))
       (write-wordindexed/raw fin sb-vm:funcallable-instance-trampoline-slot
                              (lookup-assembler-reference 'sb-vm::funcallable-instance-tramp))
       (write-wordindexed fin sb-vm::funcallable-instance-function-slot
                          ;; its discriminating function, as host PCL left it
                          (host-pcl-slot-value (or (sb-pcl::host-funcallable-function record)
                                                   (sb-pcl::host-fin-target-function record))
                                               fin :fin-function 0))
       (write-wordindexed fin sb-vm::funcallable-instance-layout-slot
                          (host-pcl-to-core (sb-pcl::host-fin-wrapper record)))
       (write-wordindexed fin sb-vm:funcallable-instance-info-offset
                          (host-pcl-to-core (host-pcl-dumped-slots
                                             (sb-pcl::host-fin-wrapper record)
                                             (sb-pcl::host-fin-slots record))))
       (write-wordindexed fin (1+ sb-vm:funcallable-instance-info-offset)
                          (make-fixnum-descriptor (sb-pcl::host-fin-hash record)))
       fin))
    ;; a layout: one of PCL's classes (a wrapper, its slots as host PCL set
    ;; them), or one of the cross-compiler's, which genesis may have already
    ((typep x 'sb-kernel:layout)
     (let* ((pcl (sb-pcl::host-wrapper-p x))
            (name (sb-kernel::layout-classoid-name x))
            (cold (gethash name *cold-layouts*)))
       (if (and cold (not pcl))
           (cold-layout-descriptor cold)
           (let* ((flags (sb-kernel:layout-flags x))
                  ;; as %UPDATE-LISP-CLASS-LAYOUT set them
                  (inherits (host-pcl-to-core (sb-kernel:layout-inherits x)))
                  (layout (make-cold-layout
                           name (sb-kernel:layout-depthoid x) flags
                           (sb-kernel:layout-length x) (sb-kernel:layout-bitmap x)
                           inherits
                           ;; layout IDs are for structure types
                           (logtest flags +structure-layout-flag+)
                           ;; A class's current layout is the one of its name
                           ;; that compiled code refers to (TYPEP);
                           ;; class-init.lisp made it.  (Looked up after
                           ;; the inherits, which may lead to it.)
                           (and pcl (eq x (sb-kernel:classoid-layout (sb-kernel:layout-classoid x)))
                                (gethash name *cold-layouts*))))
                  (index (dsd-index-cached 'sb-kernel:layout 'sb-kernel::slot-table))
                  (table (sb-kernel::layout-slot-table x)))
             (setf (gethash x *host-pcl-memo*) layout)
             (when pcl
               (pushnew name *cold-pcl-wrapper-names*)
               (write-slots layout
                            :classoid (host-pcl-classoid-to-core (sb-kernel:layout-classoid x))
                            :%info (host-pcl-to-core (sb-kernel::layout-%info x))))
             ;; its slot table: PCL's, or, as MAKE-LAYOUT's for a built-in
             ;; classoid (which cold init's LOAD-LAYOUT finds made,
             ;; *!INITIAL-LAYOUTS*), the empty one
             (cond (table
                    (write-wordindexed layout (+ sb-vm:instance-slots-offset index)
                                       (host-pcl-slot-value table layout :instance index)))
                   ((not pcl)
                    (write-wordindexed layout (+ sb-vm:instance-slots-offset index)
                                       (if (boundp '*vacuous-slot-table*)
                                           *vacuous-slot-table*
                                           (setq *vacuous-slot-table*
                                                 (host-constant-to-core '#(1 nil)))))))
             layout))))
    ((typep x 'sb-kernel:classoid) (host-pcl-classoid-to-core x))
    ;; a method combination's generic functions (METHOD-COMBINATION-GF-SET)
    ((sb-pcl::host-gf-hashset-p x)
     (host-pcl-to-core (sb-pcl::host-gf-hashset-members x)))
    ((host-pcl-mutex-p x)
     (let ((mutex (allocate-struct-of-type 'sb-thread:mutex)))
       (setf (gethash x *host-pcl-memo*) mutex)
       (write-slots mutex :name (host-pcl-to-core (host-pcl-mutex-name x)))))
    ((and (typep x 'structure-object) (get (type-of x) 'dd-proxy))
     ;; a host instance of a target defstruct, e.g. SB-PCL::SLOT-INFO
     (let ((result (allocate-struct-of-type (type-of x))))
       (setf (gethash x *host-pcl-memo*) result)
       (when (typep x 'sb-pcl::pv-table) ; the target interns it too
         (push result *host-pcl-pv-tables*))
       (loop for dsd across (type-dd-slots-or-lose (type-of x)) finally (return result) do
         (let ((index (dsd-index dsd)))
           (aver (eq (dsd-raw-type dsd) t))
           (write-wordindexed result (+ sb-vm:instance-slots-offset index)
                              (host-pcl-slot-value
                               (if (member (dsd-accessor-name dsd) *host-pcl-reset-struct-slots*)
                                   nil
                                   (funcall (dsd-accessor-name dsd) x))
                               result :instance index))))))
    (t (error "host PCL: can't write ~S" x))))

;;; What PCL's toplevel forms installed on the host as global functions:
;;; generic functions by their names, and functions installed under other
;;; names (LOAD-DEFCLASS is REAL-LOAD-DEFCLASS, ...).
;;; Install FUNCTION, a host object, as the target's definition of NAME.  PCL
;;; proper's toplevel forms must not DEFUN the name too, or cold init would
;;; replace it.
(defun host-pcl-fset (name function)
  (let ((name name))
    (if (gethash name *cold-pcl-defun-names*)
        (error "host PCL: ~S is also DEFUNed by PCL's toplevel forms" name)
        (let ((fdefn (ensure-cold-fdefn (if (symbolp name) (cold-intern name) (host-pcl-to-core name))))
              (des (host-pcl-to-core function)))
          #+linkage-space (error "host PCL: FSET ~S for linkage space" name)
          (write-wordindexed fdefn sb-vm:fdefn-fun-slot des)
          ;; as SET-FDEFN-FUN does
          (write-wordindexed/raw
           fdefn sb-vm:fdefn-raw-addr-slot
           (if (= (descriptor-widetag des) sb-vm:simple-fun-widetag)
               (+ (descriptor-base-address des) (ash sb-vm:simple-fun-insts-offset sb-vm:word-shift))
               (lookup-assembler-reference 'sb-vm::closure-tramp)))))))

(defun host-pcl-fdefinitions ()
  (let ((gfs nil))
    (maphash (lambda (fin record)
               (when (and (sb-pcl::host-fin-p record) (host-call sb-pcl::generic-function-p fin))
                 (if (host-call sb-pcl::standard-generic-function-p fin)
                     (push fin gfs)
                     (format t "~&; host PCL: not a standard generic function: ~S~%"
                             (sb-xc:class-name (sb-xc:class-of fin))))
                 (when (and (host-call sb-pcl::standard-generic-function-p fin) ; not anonymous
                            (sb-xc:slot-boundp fin 'sb-pcl::name))
                   (host-pcl-fset (host-call sb-mop:generic-function-name fin) fin))))
             sb-pcl::*host-funcallables*)
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
                   (host-pcl-fset name f))))))
      ;; host PCL's own names, and its shadows of other packages'
      (dolist (package '("SB-PCL"))
        (do-symbols (s package)
          (when (and (eq (cl:symbol-package s) (find-package package))
                     (not (search "HOST" (symbol-name s))))
            (alias s)
            (alias `(setf ,s))))))
    gfs))

(defun host-pcl-dump ()
  (let ((*host-pcl-memo* (make-hash-table :test 'eq))
        (*host-pcl-pv-tables* nil)
        (*host-pcl-slot-names* (make-hash-table :test 'eq))
        (gfs nil))
    ;; FIND-CLASS: the class in its name's classoid cell, as (SETF FIND-CLASS)
    ;; puts it before the boot state is COMPLETE
    (loop for (name . class) in (sb-pcl::host-pcl-classes)
          do (write-slots (cold-find-classoid-cell name :create t)
                          :pcl-class (host-pcl-to-core class)))
    (dolist (classoid (host-pcl-classoids))
      (when (sb-kernel:classoid-pcl-class classoid)
        (if (or (sb-pcl::host-own-classoid-p classoid)
                (host-pcl-cold-classoid (classoid-name classoid))
                (typep classoid '(or sb-kernel:built-in-classoid
                                  sb-kernel:structure-classoid
                                  sb-kernel::condition-classoid sb-kernel::static-classoid)))
            (host-pcl-classoid-to-core classoid)
            (host-pcl-unwritable (sb-kernel:classoid-pcl-class classoid)))))
    ;; slot tables of layouts that are not PCL's (STRUCTURE-OBJECT etc.)
    (dolist (classoid (host-pcl-classoids))
      (let* ((xc-layout (sb-kernel:classoid-layout classoid))
             (table (and xc-layout (not (sb-pcl::host-wrapper-p xc-layout))
                         (sb-kernel::layout-slot-table xc-layout)))
             (cold (and table (gethash (classoid-name classoid) *cold-layouts*))))
        (when table
          (if cold
              (let ((index (dsd-index-cached 'sb-kernel:layout 'sb-kernel::slot-table))
                    (layout (cold-layout-descriptor cold)))
                (write-wordindexed layout (+ sb-vm:instance-slots-offset index)
                                   (host-pcl-slot-value table layout :instance index)))
              (host-pcl-unwritable table)))))
    ;; ... and their slot mappers
    (loop for (name . mapper) in sb-pcl::*host-layout-slot-mappers*
          for cold = (gethash name *cold-layouts*)
          do (if cold
                 (let ((index (dsd-index-cached 'sb-kernel:layout 'sb-kernel::slot-mapper)))
                   (write-wordindexed (cold-layout-descriptor cold)
                                      (+ sb-vm:instance-slots-offset index)
                                      (host-pcl-to-core mapper)))
                 (host-pcl-unwritable mapper)))
    (dolist (classoid (host-pcl-classoids))
      (when (and (sb-pcl::host-own-classoid-p classoid)
                 (eq (sb-kernel:find-classoid (classoid-name classoid) nil) classoid))
        (host-pcl-to-core classoid))) ; filed in its cell
    ;; Each classoid PCL made is a subclassoid of its layout's ancestors'
    ;; classoids, as REGISTER-LAYOUT makes it (ADD-SUBCLASSOID's alist: the
    ;; target makes a hash table of a long one when it adds to it).
    (let ((subclassoids (make-hash-table :test 'eq))) ; super -> ((sub . layout) ...)
      (dolist (classoid (host-pcl-classoids))
        (let ((layout (and (sb-pcl::host-own-classoid-p classoid)
                           (sb-kernel:classoid-layout classoid))))
          (when layout
            (loop for super across (sb-kernel:layout-inherits layout)
                  do (push (cons classoid layout)
                           (gethash (sb-kernel:layout-classoid super)
                                    subclassoids))))))
      (maphash (lambda (super subs)
                 (let ((cold (host-pcl-to-core super)))
                   (write-slots cold :subclasses
                                (list-to-core
                                 (append
                                  (mapcar (lambda (sub)
                                            (cold-cons (host-pcl-to-core (car sub))
                                                       (host-pcl-to-core (cdr sub))))
                                          subs)
                                  ;; any other the cross-compiler's classoid
                                  ;; has in the core (its own of the
                                  ;; meta-braid's, class-init.lisp, are above)
                                  (let ((old (read-slot cold :subclasses))
                                        (written (mapcar (lambda (sub) (host-pcl-to-core (car sub)))
                                                         subs)))
                                    (loop until (cold-null old)
                                          unless (member (cold-car (cold-car old)) written
                                                         :test #'descriptor=)
                                            collect (cold-car old)
                                          do (setq old (cold-cdr old)))))))))
               subclassoids))
    ;; global functions and variables
    (setq gfs (host-pcl-fdefinitions))
    (dolist (symbol (remove-if (lambda (s)
                                 (member s *host-pcl-lazy-tables*))
                               sb-pcl::*host-pcl-globals*))
      (let ((value (symbol-value symbol)))
        (if (host-pcl-recipe-of value)
            (host-pcl-unwritable value) ; a hash table, say: a lazy table instead
            (cold-set symbol (host-pcl-to-core value)))))
    (cold-set 'sb-pcl::*!braid-dumped* (cold-intern t))
    (cold-set 'sb-pcl::*!genesis-pv-tables* (list-to-core *host-pcl-pv-tables*))
    (dolist (symbol *host-pcl-lazy-tables*) (cold-set symbol *nil-descriptor*))
    ;; the function generators host PCL made, which SB-XC:COMPILE compiled
    (cold-set 'sb-pcl::*!genesis-fgens*
              (let ((fgens (symbol-value (find-symbol "*FGENS*" "SB-PCL")))
                    (entries nil))
                (when fgens
                  (maphash (lambda (test fgen) (push (cons test fgen) entries)) fgens))
                (host-pcl-to-core entries)))
    (cold-set 'sb-pcl::*!genesis-table-entries*
              (host-pcl-to-core
               (loop for name in *host-pcl-genesis-tables*
                     for table = (symbol-value name)
                     when table
                       collect name
                       and collect (let (entries)
                                     (maphash (lambda (k v) (push v entries) (push k entries))
                                              table)
                                     entries))))
    (format t "~&; host PCL dumped: ~D objects, ~D generic functions~%"
            (hash-table-count *host-pcl-memo*) (length gfs))
    (format t "~&; host PCL: ~D functions cross-compiled (SB-XC:COMPILE), ~D of them written~%"
            *cold-compile-count* *host-pcl-cold-compiled-written*)))

;;;; SB-XC:COMPILE

;;; SB-XC:COMPILE of host PCL (*HOST-PCL-COLD-COMPILE*): cross-compile FORM,
;;; a lambda, and load it into the cold core now.  FOP-TEMPLATE-CLOSURE tells
;;; genesis of its function, named (TEMPLATE :COLD-COMPILE n), and, if FORM is
;;; a generator (LAMBDA parameters ... #'(LAMBDA ...)), of the function of
;;; its closures, (TEMPLATE :COLD-COMPILE n :CLOSURE).  The fasl only carries
;;; the code from the dumper to genesis's loader.
(defun cold-compile (form)
  (let* ((n (incf *cold-compile-count*))
         (name `(sb-pcl::template :cold-compile ,n))
         (parameters nil)
         (closure-name nil))
    (when (eq (car form) 'named-lambda)      ; its name is NAME
      (setq form `(lambda ,@(cddr form))))
    (aver (eq (car form) 'lambda))
    (setq parameters (second form))
    (let* ((body (cddr form))
           (inner (car (last body)))
           ;; the closure it makes: (FUNCTION (LAMBDA ...)), or (LAMBDA ...) or
           ;; (NAMED-LAMBDA ...) (fngen.lisp's)
           (lambda (if (typep inner '(cons (eql function) (cons cons null))) (second inner) inner)))
      (when (typep lambda '(cons (member lambda named-lambda)))
        (progn
          (setq closure-name (append name '(:closure)))
          (setq form `(lambda ,(second form)
                        ,@(butlast body)
                        (function (named-lambda ,closure-name
                                    ,@(if (eq (car lambda) 'lambda)
                                          (cdr lambda)
                                          (cddr lambda)))))))))
    (let ((fasl (sb-cold::stem-object-path "output/cold-compile" nil :target-compile)))
      (ensure-directories-exist fasl)
      (funcall sb-cold::*in-target-compilation-mode-fn*
               (lambda ()
                 (let ((genesis-readonlyize (fdefinition 'logically-readonlyize))
                       (sb-c::*last-message-count* (list* 0 nil nil))
                       (sb-c::*last-error-context* nil)
                       (sb-c::*compiler-trace-output* nil)
                       (sb-c::*block-compile-argument* nil)
                       (sb-c::*entry-points-argument* nil)
                       (sb-c::*force-system-tlab* t) ; as PCL's code
                       ;; it may run before cold init's toplevel forms
                       (sb-c::*load-time-value-of-global-read* t)
                       (output (sb-c::open-fasl-output (pathname fasl) "SB-XC:COMPILE"))
                       (abort-p t))
                   (unwind-protect
                        (let ((sb-c::*compile-object* output))
                          ;; the compiler's, not genesis's (which works on cold
                          ;; objects)
                          (setf (fdefinition 'logically-readonlyize) #'identity)
                          (setq abort-p (sb-c::sub-compile-file
                                         (sb-c::make-lisp-source-info
                                          `(named-lambda ,name ,@(cdr form)))
                                         nil)))
                     (setf (fdefinition 'logically-readonlyize) genesis-readonlyize)
                     (sb-c::close-fasl-output output abort-p))
                   (when abort-p (error "host PCL: failed to cross-compile ~S" form)))))
      (let ((*cold-load-functions-only* t))
        (cold-load fasl nil nil)))
    (values name closure-name parameters)))

