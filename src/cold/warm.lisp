;;;; "warm initialization": initialization which comes after cold init

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "COMMON-LISP-USER")


(defvar *warm-object-file-names*)
(defvar *warm-obj-prefix* "obj/warm/")

;;;; general warm init compilation policy

(proclaim '(optimize (compilation-speed 1)
                     (debug #+sb-show 2 #-sb-show 1)
                     (inhibit-warnings 2)
                     (safety 2)
                     (space 1)
                     (speed 2)))

;;; Assert that genesis preserved shadowing symbols.
(let ((p sb-assem::*backend-instruction-set-package*))
  (unless (eq p (find-package "SB-VM"))
    (dolist (expect '("SEGMENT" "MAKE-SEGMENT"))
      (assert (find expect (package-shadowing-symbols p) :test 'string=)))))


;;;; compiling and loading more of the system

(load "src/cold/muffler.lisp")

(unless (member sb-int:+empty-ht-slot+ sb-vm::*static-symbols*)
  ;; It doesn't "just work" to unintern the marker symbol, because then
  ;; then compiler thinks that equivalence-as-constant for such symbol permits
  ;; creation of new uninterned symbol at load-time, never mind that it was
  ;; accessed by way of a named global constant. Changing +EMPTY-HT-SLOT+
  ;; into a macro that explicitly calls LOAD-TIME-VALUE makes it work out.
  ;; I didn't want to think about getting this right in cold-init though.
  (setf (sb-int:info :variable :macro-expansion 'sb-int:+empty-ht-slot+)
        '(load-time-value (symbol-global-value 'sb-int:+empty-ht-slot+) t))
  ;; Sneaky! Now it's both a constant and a macro
  (setf (sb-int:info :variable :kind 'sb-int:+empty-ht-slot+) :macro))

(unintern sb-int:+empty-ht-slot+ (symbol-package sb-int:+empty-ht-slot+))

;;; FIXME: CMU CL's pclcom.lisp had extra optional stuff wrapped around
;;; COMPILE-PCL, at least some of which we should probably have too:
;;;
;;; (with-compilation-unit
;;;     (:optimize '(optimize (debug #+(and (not high-security) small) .5
;;;                               #-(or high-security small) 2
;;;                               #+high-security 3)
;;;                        (speed 2) (safety #+(and (not high-security) small) 0
;;;                                          #-(or high-security small) 2
;;;                                          #+high-security 3)
;;;                        (inhibit-warnings 2))
;;;      :optimize-interface '(optimize-interface #+(and (not high-security) small)
;;; (safety 1)
;;;                                            #+high-security (safety 3))
;;;      :context-declarations
;;;      '((:external (declare (optimize-interface (safety #-high-security 2 #+high-
;;; security 3)
;;;                                             (debug #-high-security 1 #+high-s
;;; ecurity 3))))
;;;     ((:or :macro (:match "$EARLY-") (:match "$BOOT-"))
;;;     (declare (optimize (speed 0))))))

(load "src/cold/read-from-file.lisp")

(defun target-platform-name ()
  (let ((arch (intersection '(:alpha :arm :arm64 :hppa :mips :ppc :sparc :x86 :x86-64)
                            *features*)))
    (cond ((not arch) (error "No architecture selected"))
          ((> (length arch) 1) (error "More than one architecture selected")))
    (string-downcase (car arch))))

(defun blank-shebang-reader (stream sub-character infix-parameter)
  (declare (ignore sub-character))
  (when infix-parameter
    (error "illegal read syntax: #~D!" infix-parameter))
  (let ((next-char (read-char stream)))
    (unless (find next-char "+-")
      (error "illegal read syntax: #!~C" next-char))
    (let ((*read-suppress* t))
      (read stream t nil t)
      (values))))
(compile 'blank-shebang-reader)

;; The build order file contains shebang feature readers. Ignore them.
(let ((*readtable* *readtable*))
  (set-dispatch-macro-character #\# #\! #'blank-shebang-reader)
  (load "src/cold/stems.lisp"))

;; compile and warm load
(let ((*compile-print* nil))
  (proclaim '(sb-ext:muffle-conditions
              (or (satisfies unable-to-optimize-note-p)
                  (satisfies optional+key-style-warning-p))))
  (setf *warm-object-file-names*
        (let ((reversed-target-object-file-names nil))
          (with-compilation-unit ()
            (do-stems-and-flags (stem flags)
              (when (member :warm flags)
                (let ((filename (compile-stem stem flags :warm-compile)))
                  (load filename)
                  (push filename reversed-target-object-file-names)))))
          (nreverse reversed-target-object-file-names))))
