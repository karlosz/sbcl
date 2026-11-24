;;;; single stepper for SBCL

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMUCL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;;; Single stepping is driven by breakpoints at the code locations
;;;; the compiler notes, with a handler for the conditions they signal
;;;; at the toplevel.

(in-package "SB-IMPL")

#-sb-xc-host
(progn

(defun enable-stepping ()
  ;; The breakpoint based stepper starts stepping from the frame we
  ;; are currently stopped in; it is loaded later than this file.
  (when (fboundp 'sb-debug::%enable-stepping)
    (sb-debug::%enable-stepping)))
(defun disable-stepping ()
  ;; Stop following execution: clear the breakpoints the stepper
  ;; armed.  It is loaded later than this file.
  (when (fboundp 'sb-debug::clear-step-breakpoints)
    (sb-debug::clear-step-breakpoints)))

) ; end PROGN
