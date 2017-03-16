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

;; make sure the target does not keep #!+- readers
(let ((*readtable* (copy-readtable)))
  (load "src/cold/stems.lisp"))

(setf *warm-obj-prefix* "obj/warm/")

;; compile and warm load
(setf *warm-object-file-names*
      (let ((reversed-target-object-file-names nil)
            ;; make sure warm init muffled conditions dont leak into final image
            (sb-c::*handled-conditions* sb-c::*handled-conditions*))
        (declaim (sb-ext:muffle-conditions
                  (or (satisfies unable-to-optimize-note-p)
                      (satisfies optional+key-style-warning-p))))
        (with-compilation-unit ()
          (do-stems-and-flags (stem flags)
            (when (member :warm flags)
              ;; we can do better, split it up into 2 stages instead of this spec var
              (let ((filename (ecase (if (boundp '*compile-files-p*)
                                      *compile-files-p*
                                      t)
                                ((t) (compile-stem stem flags :warm-compile))
                                ((nil) (stem-object-path stem flags :warm-compile)))))
                (load filename)
                (push filename reversed-target-object-file-names)))))
        (nreverse reversed-target-object-file-names)))
