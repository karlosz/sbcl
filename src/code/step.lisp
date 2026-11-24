;;;; single stepper for SBCL

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMUCL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;;; Single stepping signals STEP-CONDITIONs from breakpoints at the
;;;; code locations the compiler steps, and has a handler for them at
;;;; the toplevel.

(in-package "SB-IMPL")

(defgeneric single-step (condition))

(defmethod single-step ((condition step-values-condition))
  (let ((values (step-condition-result condition)))
    (format *debug-io* "; ~A => ~:[#<no value>~;~{~S~^, ~}~]~%"
            (step-condition-form condition)
            values values)))

(defmethod single-step ((condition step-form-condition))
  (let ((form (step-condition-form condition))
        (args (step-condition-args condition)))
    (let ((*print-circle* t)
          (*print-pretty* t)
          (*print-readably* nil))
      (format *debug-io*
              "; Evaluating call:~%~<; ~@;  ~A~:>~%~
               ; ~:[With arguments:~%~<; ~@;~{  ~S~^~%~}~:>~;With unknown arguments~]~%"
              (list form)
              (eq args :unknown)
              (list args)))
    (finish-output *debug-io*)
    (let ((sb-debug:*stack-top-hint* (sb-di::find-stepped-frame)))
      (invoke-debugger condition))))

;;; In the TTY debugger we're not interested in STEP returning
(defmethod single-step ((condition step-finished-condition))
  (values))

(defvar *stepper-hook* 'single-step
  "Customization hook for alternative single-steppers.
*STEPPER-HOOK* is bound to NIL prior to calling the bound function
with the STEP-CONDITION as argument.")

(defun invoke-stepper (condition)
  (when *stepper-hook*
    (let ((hook *stepper-hook*)
          (*stepper-hook* nil))
      (funcall hook condition))))

(defmacro step (form)
  "The form is evaluated with single stepping enabled. Function calls
outside the lexical scope of the form can be stepped into only if the
functions in question have been compiled with sufficient DEBUG policy
to be at least partially steppable."
  `(sb-debug::step-internal (named-lambda "stepping lambda" ()
                              ;; Debug 1 makes the locations the
                              ;; stepper needs be dumped even when the
                              ;; STEP form sits in low debug code.
                              (declare (optimize (debug 1)
                                                 (sb-c:insert-step-conditions 3)))
                              ,form)
                            ',form))

