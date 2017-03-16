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


(load "src/cold/muffler.lisp")
(declaim (sb-ext:muffle-conditions
          (satisfies unable-to-optimize-note-p)
          (satisfies optional+key-style-warning-p)
          sb-ext:code-deletion-note))

(load "src/cold/stems.lisp")
