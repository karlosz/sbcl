;;;; stuff which is not specific to any particular build phase, but
;;;; used by most of them
;;;;
;;;; Note: It's specifically not used when bootstrapping PCL, because
;;;; we do SAVE-LISP after that, and we don't want to save extraneous
;;;; bootstrapping machinery into the frozen image which will
;;;; subsequently be used as the mother of all Lisp sessions.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;; SB-COLD holds stuff used to build the initial SBCL core file
;;; (including not only the final construction of the core file, but
;;; also the preliminary steps like e.g. building the cross-compiler
;;; and running the cross-compiler to produce target FASL files).
(defpackage "SB-COLD" (:use "CL"))

(in-package "SB-COLD")

(defun parse-make-host-parallelism (str)
  (multiple-value-bind (value1 end) (parse-integer str :junk-allowed t)
    (when value1
      (let ((value2 (if (and value1
                             (< end (1- (length str))) ; ~ /,[\d]+/
                             (eql (char str end) #\,))
                        (parse-integer str :start (1+ end)))))
        ;; If only 1 integer, assume same parallelism for both passes.
        (unless value2
          (setq value2 value1))
        ;; 0 means no parallelism. 1 means use at most one subjob,
        ;; just in case you want to test the controlling loop.
        (when (eql value1 0) (setq value1 nil))
        (when (eql value2 0) (setq value2 nil))
        ;; Parallelism on pass 1 works only if LOAD does not compile.
        ;; Otherwise it's slower than compiling serially.
        ;; (And this has only been tested with sb-fasteval, not sb-eval.)
        (cons (and (find-package "SB-INTERPRETER") value1)
              value2)))))

(defvar *make-host-parallelism*
  (or #+sbcl
      (let ((envvar (sb-ext:posix-getenv "SBCL_MAKE_PARALLEL")))
        (when envvar
          (parse-make-host-parallelism envvar)))))

(defun make-host-1-parallelism () (car *make-host-parallelism*))
(defun make-host-2-parallelism () (cdr *make-host-parallelism*))


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

;;; other miscellaneous tools
(load "src/cold/read-from-file.lisp")
(load "src/cold/rename-package-carefully.lisp")
(load "src/cold/with-stuff.lisp")

;;; Try to minimize/conceal any non-standardness of the host Common Lisp.
(load "src/cold/ansify.lisp")

;;;; special read-macros for building the cold system (and even for
;;;; building some of our tools for building the cold system)

(load "src/cold/shebang.lisp")

;;; When cross-compiling, the *FEATURES* set for the target Lisp is
;;; not in general the same as the *FEATURES* set for the host Lisp.
;;; In order to refer to target features specifically, we refer to
;;; *SHEBANG-FEATURES* instead of *FEATURES*, and use the #!+ and #!-
;;; readmacros instead of the ordinary #+ and #- readmacros.
(setf *shebang-features*
      (let* ((default-features
               (funcall (compile
                         nil
                         (read-from-file "local-target-features.lisp-expr"))
                        (read-from-file "base-target-features.lisp-expr")))
             (customizer-file-name "customize-target-features.lisp")
             (customizer (if (probe-file customizer-file-name)
                             (compile nil
                                      (read-from-file customizer-file-name))
                             #'identity)))
        (funcall customizer default-features)))
(let ((*print-length* nil)
      (*print-level* nil))
  (format t
          "target features *SHEBANG-FEATURES*=~%~@<~S~:>~%"
          *shebang-features*))

(defvar *shebang-backend-subfeatures*
  (let* ((default-subfeatures nil)
         (customizer-file-name "customize-backend-subfeatures.lisp")
         (customizer (if (probe-file customizer-file-name)
                         (compile nil
                                  (read-from-file customizer-file-name))
                         #'identity)))
    (funcall customizer default-subfeatures)))
(let ((*print-length* nil)
      (*print-level* nil))
  (format t
          "target backend-subfeatures *SHEBANG-BACKEND-FEATURES*=~@<~S~:>~%"
          *shebang-backend-subfeatures*))

;;; Call for effect of signaling an error if no target picked.
(target-platform-name)

;;; You can get all the way through make-host-1 without either one of these
;;; features, but then 'bit-bash' will fail to cross-compile.
(unless (intersection '(:big-endian :little-endian) *shebang-features*)
  (warn "You'll have bad time without either endian-ness defined"))

;;; Some feature combinations simply don't work, and sometimes don't
;;; fail until quite a ways into the build.  Pick off the more obvious
;;; combinations now, and provide a description of what the actual
;;; failure is (not always obvious from when the build fails).
(let ((feature-compatibility-tests
       '(("(and sb-thread (not gencgc))"
          ":SB-THREAD requires :GENCGC")
         ("(and sb-thread (not (or ppc x86 x86-64 arm64)))"
          ":SB-THREAD not supported on selected architecture")
         ("(and gencgc cheneygc)"
          ":GENCGC and :CHENEYGC are incompatible")
         ("(and cheneygc (not (or alpha arm hppa mips ppc sparc)))"
          ":CHENEYGC not supported on selected architecture")
         ("(and gencgc (not (or sparc ppc x86 x86-64 arm arm64)))"
          ":GENCGC not supported on selected architecture")
         ("(not (or gencgc cheneygc))"
          "One of :GENCGC or :CHENEYGC must be enabled")
         ("(and sb-dynamic-core (not linkage-table))"
          ":SB-DYNAMIC-CORE requires :LINKAGE-TABLE")
         ("(and sb-linkable-runtime (not sb-dynamic-core))"
          ":SB-LINKABLE-RUNTIME requires :SB-DYNAMIC-CORE")
         ("(and sb-linkable-runtime (not (or x86 x86-64)))"
          ":SB-LINKABLE-RUNTIME not supported on selected architecture")
         ("(and sb-linkable-runtime (not (or darwin linux win32)))"
          ":SB-LINKABLE-RUNTIME not supported on selected operating system")
         ("(and sb-eval sb-fasteval)"
          ;; It sorta kinda works to have both, but there should be no need,
          ;; and it's not really supported.
          "At most one interpreter can be selected")
         ("(and immobile-space (not x86-64))"
          ":IMMOBILE-SPACE is supported only on x86-64")
        ("(and compact-instance-header (not immobile-space))"
          ":COMPACT-INSTANCE-HEADER requires :IMMOBILE-SPACE feature")
        ("(and immobile-code (not immobile-space))"
          ":IMMOBILE-CODE requires :IMMOBILE-SPACE feature")
        ("(and immobile-symbols (not immobile-space))"
          ":IMMOBILE-SYMBOLS requires :IMMOBILE-SPACE feature")
         ;; There is still hope to make multithreading on DragonFly x86-64
         ("(and sb-thread x86 dragonfly)"
          ":SB-THREAD not supported on selected architecture")))
      (failed-test-descriptions nil))
  (dolist (test feature-compatibility-tests)
    (let ((*features* *shebang-features*))
      (when (read-from-string (concatenate 'string "#+" (first test) "T NIL"))
        (push (second test) failed-test-descriptions))))
  (when failed-test-descriptions
    (error "Feature compatibility check failed, ~S"
           failed-test-descriptions)))

;;;; cold-init-related PACKAGE and SYMBOL tools

;;; Once we're done with possibly ANSIfying the COMMON-LISP package,
;;; it's probably a mistake if we change it (beyond changing the
;;; values of special variables such as *** and +, anyway). Set up
;;; machinery to warn us when/if we change it.
;;;
;;; All code depending on this is itself dependent on #!+SB-SHOW.
#!+sb-show
(progn
  (load "src/cold/snapshot.lisp")
  (defvar *cl-snapshot* (take-snapshot "COMMON-LISP")))

(load "src/cold/stems.lisp")
