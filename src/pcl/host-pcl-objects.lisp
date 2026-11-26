;;;; What host PCL built, for genesis to write into the cold core.
;;;;
;;;; make-host-2 compiles this file once every target file is compiled and
;;;; host PCL's build is over (HOST-PCL-COMPILE-OBJECTS, in
;;;; src/cold/compile-cold-sbcl.lisp), and genesis loads it once it has
;;;; loaded them (src/cold/host-pcl-dump.lisp).

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

(in-package "SB-PCL")

;;; The functions host PCL compiled (SB-XC:COMPILE), of which its objects
;;; are closures: genesis knows each by its name (FOP-TEMPLATE-CLOSURE)
(!host-form (lambda () (!host-pcl-compiled-functions)))

;;; ... and the objects: a constant that host PCL writes into the fasl as the
;;; dumper meets it (SB-C::DEFERRED-DUMP)
(!host-form (!host-pcl-objects))
