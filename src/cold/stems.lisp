;;;; this file holds stem loading machinery shared between all build
;;;; phases. this file has no package because it will be in SB-COLD
;;;; on the cross compiler but in a target accessible package on the
;;;; target during warm load.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;; If TRUE, then COMPILE-FILE is being invoked only to process
;;; :COMPILE-TOPLEVEL forms, not to produce an output file.
;;; This is part of the implementation of parallelized make-host-2.
(defvar *compile-for-effect-only* nil)

;;; prefixes for filename stems when cross-compiling. These are quite arbitrary
;;; (although of course they shouldn't collide with anything we don't want to
;;; write over). In particular, they can be either relative path names (e.g.
;;; "host-objects/" or absolute pathnames (e.g. "/tmp/sbcl-xc-host-objects/").
;;;
;;; The cross-compilation process will force the creation of these directories
;;; by executing CL:ENSURE-DIRECTORIES-EXIST (on the xc host Common Lisp).
(defvar *host-obj-prefix*)
(defvar *target-obj-prefix*)
(defvar *warm-obj-prefix*)


(defvar *target-obj-suffix*
  ;; Target fasl files are LOADed (actually only quasi-LOADed, in
  ;; GENESIS) only by SBCL code, and it doesn't care about particular
  ;; extensions, so we can use something arbitrary.
  ".lisp-obj")
(defvar *target-assem-obj-suffix*
  ;; Target fasl files from SB!C:ASSEMBLE-FILE are LOADed via GENESIS.
  ;; The source files are compiled once as assembly files and once as
  ;; normal lisp files.  In the past, they were kept separate by
  ;; clever symlinking in the source tree, but that became less clean
  ;; as ports to host environments without symlinks started appearing.
  ;; In order to keep them separate, we have the assembled versions
  ;; with a separate suffix.
  ".assem-obj")

;;; a function of one functional argument, which calls its functional argument
;;; in an environment suitable for compiling the target. (This environment
;;; includes e.g. a suitable *FEATURES* value.)
(declaim (type function *in-target-compilation-mode-fn*))
(defvar *in-target-compilation-mode-fn*)

;;; a function with the same calling convention as CL:COMPILE-FILE, to be
;;; used to translate ordinary Lisp source files into target object files
(declaim (type function *target-compile-file*))
(defvar *target-compile-file*)

;;; designator for a function with the same calling convention as
;;; SB-C:ASSEMBLE-FILE, to be used to translate assembly files into target
;;; object files
(defvar *target-assemble-file*)

;;;; master list of source files and their properties

;;; flags which can be used to describe properties of source files
(defparameter
  *expected-stem-flags*
  '(;; meaning: This file is not to be compiled when building the
    ;; cross-compiler which runs on the host ANSI Lisp. ("not host
    ;; code", i.e. does not execute on host -- but may still be
    ;; cross-compiled by the host, so that it executes on the target)
    :not-host
    ;; meaning: This file is not to be compiled as part of the target
    ;; SBCL. ("not target code" -- but still presumably host code,
    ;; used to support the cross-compilation process)
    :not-target
    ;; meaning: Load this file during warm init, when the target
    ;; compiler is up and running. Implies both :not-host and :not-target
    :warm
    ;; meaning: This file must always be compiled by 'slam.lisp' even if
    ;; the object is not out of date with respect to its source.
    ;; Necessary if there are compile-time-too effects that are not
    ;; reflected into make-host-2 by load-time actions of make-host-1.
    :slam-forcibly
    ;; meaning: The #'COMPILE-STEM argument :TRACE-FILE should be T.
    ;; When the compiler is SBCL's COMPILE-FILE or something like it,
    ;; compiling "foo.lisp" will generate "foo.trace" which contains lots
    ;; of exciting low-level information about representation selection,
    ;; VOPs used by the compiler, and bits of assembly.
    :trace-file
    ;; meaning: This file is to be processed with the SBCL assembler,
    ;; not COMPILE-FILE. (Note that this doesn't make sense unless
    ;; :NOT-HOST is also set, since the SBCL assembler doesn't exist
    ;; while the cross-compiler is being built in the host ANSI Lisp.)
    :assem
    ;; meaning: The #'COMPILE-STEM argument called :IGNORE-FAILURE-P
    ;; should be true. (This is a KLUDGE: I'd like to get rid of it.
    ;; For now, it exists so that compilation can proceed through the
    ;; legacy warnings in src/compiler/x86/array.lisp, which I've
    ;; never figured out but which were apparently acceptable in CMU
    ;; CL. Eventually, it would be great to just get rid of all
    ;; warnings and remove support for this flag. -- WHN 19990323)
    :ignore-failure-p
    ;; meaning: Build this file, but don't put it on the list for
    ;; genesis to include in the cold core.
    :not-genesis))

(defparameter *stems-and-flags* (read-from-file "build-order.lisp-expr"))

(defvar *array-to-specialization* (make-hash-table :test #'eq))

(defmacro do-stems-and-flags ((stem flags) &body body)
  (let ((stem-and-flags (gensym "STEM-AND-FLAGS")))
    `(dolist (,stem-and-flags *stems-and-flags*)
       (let ((,stem (first ,stem-and-flags))
             (,flags (rest ,stem-and-flags)))
         ,@body
         (clrhash *array-to-specialization*)))))

;;; Given a STEM, remap the path component "/target/" to a suitable
;;; target directory.
(defun stem-remap-target (stem)
  (let ((position (search "/target/" stem)))
    (if position
      (concatenate 'string
                   (subseq stem 0 (1+ position))
                   (target-platform-name)
                   (subseq stem (+ position 7)))
      stem)))
(compile 'stem-remap-target)

;;; Determine the source path for a stem.
(defun stem-source-path (stem)
  (concatenate 'string "" (stem-remap-target stem) ".lisp"))
(compile 'stem-source-path)

;;; Determine the object path for a stem/flags/mode combination.
(defun stem-object-path (stem flags mode)
  (multiple-value-bind
        (obj-prefix obj-suffix)
      (ecase mode
        (:host-compile
         ;; On some xc hosts, it's impossible to LOAD a fasl file unless it
         ;; has the same extension that the host uses for COMPILE-FILE
         ;; output, so we have to be careful to use the xc host's preferred
         ;; extension.
         (values *host-obj-prefix*
                 (concatenate 'string "."
                              (pathname-type (compile-file-pathname stem)))))
        (:target-compile (values *target-obj-prefix*
                                 (if (find :assem flags)
                                     *target-assem-obj-suffix*
                                     *target-obj-suffix*)))
        (:warm-compile (values *warm-obj-prefix*
                               (concatenate 'string ".fasl"))))
    (concatenate 'string obj-prefix (stem-remap-target stem) obj-suffix)))
(compile 'stem-object-path)

;;; Check for stupid typos in FLAGS list keywords.
(let ((stems (make-hash-table :test 'equal)))
  (do-stems-and-flags (stem flags)
    ;; We do duplicate stem comparison based on the object path in
    ;; order to cover the case of stems with an :assem flag, which
    ;; have two entries but separate object paths for each.  KLUDGE:
    ;; We have to bind *target-obj-prefix* here because it's normally
    ;; set up later in the build process and we don't actually care
    ;; what it is so long as it doesn't change while we're checking
    ;; for duplicate stems.
    (let* ((*target-obj-prefix* "")
           (object-path (stem-object-path stem flags :target-compile)))
      (if (gethash object-path stems)
          (error "duplicate stem ~S in *STEMS-AND-FLAGS*" stem)
          (setf (gethash object-path stems) t)))

    ;; assembly files are only destined for the target
    (when (and (member :assem flags) (not (member :not-host flags)))
      (error "stem with :ASSEM flag and no :NOT-HOST flag  in *STEMS-AND-FLAGS* stem: ~S"
             stem))

    (when (and (member :warm flags) (member :not-target flags))
          (error "stem with :WARM flag but also :NOT-TARGET flag  in *STEMS-AND-FLAGS* stem: ~S"
                 stem))

    (let ((set-difference (set-difference flags *expected-stem-flags*)))
      (when set-difference
        (error "found unexpected flag(s) in *STEMS-AND-FLAGS*: ~S"
               set-difference)))))

;;;; tools to compile SBCL sources to create the cross-compiler

;;; a wrapper for compilation/assembly, used mostly to centralize
;;; the procedure for finding full filenames from "stems"
;;;
;;; Compile the source file whose basic name is STEM, using some
;;; standard-for-the-SBCL-build-process procedures to generate the
;;; full pathnames of source file and object file. Return the pathname
;;; of the object file for STEM.
;;;
;;; STEM and FLAGS are as per DO-STEMS-AND-FLAGS.  MODE is one of
;;; :HOST-COMPILE and :TARGET-COMPILE.
(defun compile-stem (stem flags mode)

  (let* (;; KLUDGE: Note that this CONCATENATE 'STRING stuff is not The Common
         ;; Lisp Way, although it works just fine for common UNIX environments.
         ;; Should it come to pass that the system is ported to environments
         ;; where version numbers and so forth become an issue, it might become
         ;; urgent to rewrite this using the fancy Common Lisp PATHNAME
         ;; machinery instead of just using strings. In the absence of such a
         ;; port, it might or might be a good idea to do the rewrite.
         ;; -- WHN 19990815
         (src (stem-source-path stem))
         (obj (stem-object-path stem flags mode))
         ;; Compile-for-effect happens simultaneously with a forked compile,
         ;; so we need the for-effect output not to stomp on the real output.
         (tmp-obj
          (concatenate 'string obj
                       (if *compile-for-effect-only* "-scratch" "-tmp")))

         (compile-file (ecase mode
                         (:host-compile #'compile-file)
                         (:target-compile (if (find :assem flags)
                                              *target-assemble-file*
                                              *target-compile-file*))
                         (:warm-compile #'compile-file)))
         (trace-file (find :trace-file flags))
         (ignore-failure-p (find :ignore-failure-p flags)))
    (declare (type function compile-file))

    (ensure-directories-exist obj :verbose t)

    ;; We're about to set about building a new object file. First, we
    ;; delete any preexisting object file in order to avoid confusing
    ;; ourselves later should we happen to bail out of compilation
    ;; with an error.
    (when (and (not *compile-for-effect-only*) (probe-file obj))
      (delete-file obj))

    ;; Original comment:
    ;;
    ;;   Work around a bug in CLISP 1999-01-08 #'COMPILE-FILE: CLISP
    ;;   mangles relative pathnames passed as :OUTPUT-FILE arguments,
    ;;   but works OK with absolute pathnames.
    ;;
    ;; following discussion on cmucl-imp 2002-07
    ;; "COMPILE-FILE-PATHNAME", it would seem safer to deal with
    ;; absolute pathnames all the time; it is no longer clear that the
    ;; original behaviour in CLISP was wrong or that the current
    ;; behaviour is right; and in any case absolutifying the pathname
    ;; insulates us against changes of behaviour. -- CSR, 2002-08-09
    (setf tmp-obj
          ;; (Note that this idiom is taken from the ANSI
          ;; documentation for TRUENAME.)
          (with-open-file (stream tmp-obj
                                  :direction :output
                                  ;; Compilation would overwrite the
                                  ;; temporary object anyway and overly
                                  ;; strict implementations default
                                  ;; to :ERROR.
                                  :if-exists :supersede)
            (close stream)
            (truename stream)))
    ;; and some compilers (e.g. OpenMCL) will complain if they're
    ;; asked to write over a file that exists already (and isn't
    ;; recognizeably a fasl file), so
    (when (probe-file tmp-obj)
      (delete-file tmp-obj))

    ;; Try to use the compiler to generate a new temporary object file.
    (flet ((report-recompile-restart (stream)
             (format stream "Recompile file ~S" src))
           (report-continue-restart (stream)
             (format stream "Continue, using possibly bogus file ~S" obj)))
      (tagbody
       retry-compile-file
         (multiple-value-bind (output-truename warnings-p failure-p)
             (restart-case
                 (if trace-file
                     (funcall compile-file src :output-file tmp-obj
                                               :trace-file t :allow-other-keys t)
                     (funcall compile-file src :output-file tmp-obj))
               (recompile ()
                 :report report-recompile-restart
                 (go retry-compile-file)))
           (declare (ignore warnings-p))
           (cond ((not output-truename)
                  (error "couldn't compile ~S" src))
                 (failure-p
                  (if ignore-failure-p
                      (warn "ignoring FAILURE-P return value from compilation of ~S"
                            src)
                      (unwind-protect
                           (restart-case
                               (error "FAILURE-P was set when creating ~S."
                                      obj)
                             (recompile ()
                               :report report-recompile-restart
                               (go retry-compile-file))
                             (continue ()
                               :report report-continue-restart
                               (setf failure-p nil)))
                        ;; Don't leave failed object files lying around.
                        (when (and failure-p (probe-file tmp-obj))
                          (delete-file tmp-obj)
                          (format t "~&deleted ~S~%" tmp-obj)))))
                 ;; Otherwise: success, just fall through.
                 (t nil)))))

    ;; If we get to here, compilation succeeded, so it's OK to rename
    ;; the temporary output file to the permanent object file.
    (cond ((not *compile-for-effect-only*)
           (rename-file-a-la-unix tmp-obj obj))
          ((probe-file tmp-obj)
           (delete-file tmp-obj))) ; clean up the trash

    ;; nice friendly traditional return value
    (pathname obj)))
(compile 'compile-stem)

;;; Execute function FN in an environment appropriate for compiling the
;;; cross-compiler's source code in the cross-compilation host.
(defun in-host-compilation-mode (fn)
    (declare (type function fn))
    (let ((*features* (cons :sb-xc-host *features*)))
      (with-additional-nickname ("SB-XC" "SB!XC")
        (funcall fn))))
(compile 'in-host-compilation-mode)

;;; Process a file as source code for the cross-compiler, compiling it
;;; (if necessary) in the appropriate environment, then loading it
;;; into the cross-compilation host Common lisp.
(defun host-cload-stem (stem flags)
  (loop
   (with-simple-restart (recompile "Recompile")
     (let ((compiled-filename (in-host-compilation-mode
                               (lambda ()
                                 (compile-stem stem flags :host-compile)))))
       (return
         (load compiled-filename))))))
(compile 'host-cload-stem)

;;; like HOST-CLOAD-STEM, except that we don't bother to compile
(defun host-load-stem (stem flags)
  (loop
   (with-simple-restart (recompile "Reload")
     (return (load (stem-object-path stem flags :host-compile))))))
(compile 'host-load-stem)

;;;; tools to compile SBCL sources to create object files which will
;;;; be used to create the target SBCL .core file

;;; Run the cross-compiler on a file in the source directory tree to
;;; produce a corresponding file in the target object directory tree.
(defun target-compile-stem (stem flags)
  (funcall *in-target-compilation-mode-fn*
           (lambda ()
             (compile-stem stem flags :target-compile))))
(compile 'target-compile-stem)

;;; (This function is not used by the build process, but is intended
;;; for interactive use when experimenting with the system. It runs
;;; the cross-compiler on test files with arbitrary filenames, not
;;; necessarily in the source tree, e.g. in "/tmp".)
(defun target-compile-file (filename)
  (funcall *in-target-compilation-mode-fn*
           (lambda ()
             (funcall *target-compile-file* filename))))
(compile 'target-compile-file)

(defun make-assembler-package (pkg-name)
  (when (find-package pkg-name)
    (delete-package pkg-name))
  (let ((pkg (make-package pkg-name
                           :use '("CL" "SB!INT" "SB!EXT" "SB!KERNEL" "SB!VM"
                                  "SB!SYS" ; for SAP accessors
                                  ;; Dependence of the assembler on the compiler
                                  ;; feels a bit backwards, but assembly needs
                                  ;; TN-SC, TN-OFFSET, etc. because the compiler
                                  ;; doesn't speak the assembler's language.
                                  ;; Rather vice-versa.
                                  "SB!C"))))
    ;; Both SB-ASSEM and SB-DISASSEM export these two symbols.
    ;; Neither is shadowing-imported. If you need one, package-qualify it.
    (shadow '("SEGMENT" "MAKE-SEGMENT") pkg)
    (use-package '("SB!ASSEM" "SB!DISASSEM") pkg)
    pkg))


;;;; some tools

;;; Take the file named X and make it into a file named Y. Sorta like
;;; UNIX, and unlike Common Lisp's bare RENAME-FILE, we don't allow
;;; information from the original filename to influence the final
;;; filename. (The reason that it's only sorta like UNIX is that in
;;; UNIX "mv foo bar/" will work, but the analogous
;;; (RENAME-FILE-A-LA-UNIX "foo" "bar/") should fail.)
;;;
;;; (This is a workaround for the weird behavior of Debian CMU CL
;;; 2.4.6, where (RENAME-FILE "dir/x" "dir/y") tries to create a file
;;; called "dir/dir/y". If that behavior goes away, then we should be
;;; able to get rid of this function and use plain RENAME-FILE in the
;;; COMPILE-STEM function above. -- WHN 19990321
(defun rename-file-a-la-unix (x y)

  (let ((path    ;; (Note that the TRUENAME expression here is lifted from an
                 ;; example in the ANSI spec for TRUENAME.)
         (with-open-file (stream y :direction :output)
           (close stream)
           ;; From the ANSI spec: "In this case, the file is closed
           ;; when the truename is tried, so the truename
           ;; information is reliable."
           (truename stream))))
    (delete-file path)
    (rename-file x path)))
(compile 'rename-file-a-la-unix)
