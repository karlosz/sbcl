(load "src/cold/shared.lisp")
(in-package "SB-COLD")
(setf *host-obj-prefix* "obj/from-host/"
      *target-obj-prefix* "obj/from-xc/")
(load "src/cold/set-up-cold-packages.lisp")
(load "src/cold/defun-load-or-cload-xcompiler.lisp")
(load-or-cload-xcompiler #'host-load-stem)
;;; The second genesis runs PCL's bootstrap on the host, which needs the
;;; cross-compiler as make-host-2 leaves it: so the target files' compile-time
;;; effects again, without writing object files (compile-cold-sbcl.lisp with
;;; no file to compile)
(load "src/cold/host-pcl-defs.lisp")
(defvar cl-user::*compile-files* '())
(load "src/cold/compile-cold-sbcl.lisp")
(load "src/cold/genesis-2.lisp")

#+cmu (ext:quit)
#+clisp (ext:quit)
#+abcl (ext:quit)
