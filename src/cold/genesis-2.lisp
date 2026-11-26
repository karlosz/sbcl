;;;; The second genesis: the cold core, with the class hierarchy that PCL
;;;; builds on the host (src/cold/host-pcl.lisp).  Run at the end of
;;;; make-host-2.lisp, where the cross-compiler can compile what host PCL
;;;; compiles, or by make-genesis-2.lisp in a fresh host Lisp.

(in-package "SB-COLD")

(load "tools-for-build/corefile.lisp" :verbose nil)
(host-cload-stem "src/compiler/generic/genesis" nil)
;;; Finish PCL's bootstrap of the class hierarchy on the host, with what the
;;; cross-compiler noted of the methods it compiled; genesis writes out
;;; what it builds, once the object files are loaded, with what host PCL
;;; compiles cross-compiled into the cold core as well
;;; (SB-FASL::COLD-COMPILE).
(load "src/cold/host-pcl-dump.lisp")
(handler-bind ((style-warning #'muffle-warning))
  (funcall (intern "HOST-PCL-BUILD" "SB-PCL")))
;;; what it compiled, which the dump refers to
(map nil #'delete-file (directory "output/cold-compile/*.fasl"))
(sb-fasl::cold-compile-host-pcl)
(setq sb-fasl::*cold-load-finish-hook* #'sb-fasl::cold-load-host-pcl-code)

(let (object-file-names foptrace-file-names)
  (do-stems-and-flags (stem flags 2)
    (unless (member :not-target flags)
      (push (stem-object-path stem flags :target-compile)
            object-file-names)
      (when (member :foptrace-file flags)
        (push (stem-object-path stem flags :target-compile)
              foptrace-file-names))))
  (setf object-file-names (nreverse object-file-names))
  (genesis :object-file-names object-file-names
           :foptrace-file-names foptrace-file-names
           :defstruct-descriptions (find-bootstrap-file "output/defstructs.lisp-expr" t)
           :tls-init (read-from-file "output/tls-init.lisp-expr" :build-dependent t)
           :c-header-dir-name "output/genesis-2"
           :core-file-name "output/cold-sbcl.core"
           ;; The map file is not needed by the system, but can be
           ;; very handy when debugging cold init problems.
           :map-file-name "output/cold-sbcl.map"))
