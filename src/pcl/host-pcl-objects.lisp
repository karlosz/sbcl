;;;; What host PCL built, for genesis to write into the cold core
;;;; (src/cold/host-pcl-dump.lisp).  The last of the cold files: host PCL's
;;;; build ends as the cross-compiler compiles it.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

(in-package "SB-PCL")

;;; (Not as a parallel build's parent compiles it for its effect: its child
;;; compiles it.)  Its code may run before cold init's toplevel forms.
(eval-when (:compile-toplevel)
  (unless sb-cold::*compile-for-effect-only*
    (host-pcl-build))
  (setq sb-c::*load-time-value-of-global-read* t))

;;; The functions host PCL compiled (SB-XC:COMPILE), of which its objects
;;; are closures: genesis knows each by its name (FOP-PCL-FUNCTION)
(!host-form (lambda () (!host-pcl-compiled-functions)))

;;; ... and the objects: a constant that host PCL writes into the fasl as the
;;; dumper meets it (SB-C::DEFERRED-DUMP)
(!host-form (!host-pcl-objects))
