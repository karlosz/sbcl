;;;; The second genesis: the cold core, with the class hierarchy that PCL
;;;; builds on the host (src/cold/host-pcl.lisp).  Run at the end of
;;;; make-host-2.lisp, where the cross-compiler can compile what host PCL
;;;; compiles, or by make-genesis-2.lisp in a fresh host Lisp.

(in-package "SB-COLD")

(load "tools-for-build/corefile.lisp" :verbose nil)
(host-cload-stem "src/compiler/generic/genesis" nil)
;;; Run PCL's bootstrap of the class hierarchy on the host, once the object
;;; files are loaded; genesis writes out what it builds.  In make-host-2,
;;; what host PCL compiles is cross-compiled into the cold core as well
;;; (SB-FASL::COLD-COMPILE).
(load "src/cold/host-pcl-dump.lisp")
(setq sb-fasl::*cold-load-finish-hook*
      (lambda ()
        ;; host PCL's hashes are the target's from here on, the dump's
        ;; lookups included
        (setq sb-c::*symbol-hash-hook*
              (lambda (symbol)
                (sb-fasl::cold-symbol-hash symbol)))
        (let ((sb-pcl::*host-pcl-cold-compile*
                ;; whose own tables have the cross-compiler's hashes
                (lambda (form)
                  (let ((sb-c::*symbol-hash-hook* nil))
                    (sb-fasl::cold-compile form)))))
          (handler-bind ((style-warning #'muffle-warning))
            (funcall (intern "HOST-PCL-BUILD" "SB-PCL"))))))

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

;;; Genesis is over: COLD-SYMBOL-HASH no longer works (after-xc.core).
(setq sb-c::*symbol-hash-hook* nil)
