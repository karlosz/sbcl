;;;; SPARC-specific runtime stuff

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMUCL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.
(in-package "SB-VM")

;;; See x86-vm.lisp for a description of this.
(defun machine-type ()
  "Return a string describing the type of the local machine."
  "SPARC")

(defun return-machine-address (scp)
  (+ (context-register scp lip-offset) 8))

;;; Return the target of the call instruction in the current signal
;;; context.  INSTRUCTION is the original instruction word at the
;;; trap, saved by the breakpoint machinery, when a breakpoint is
;;; installed at the location.
(defun context-call-site-target (context instruction)
  (declare (type (alien (* os-context-t)) context))
  (flet ((sx (value bits)
           (if (logbitp (1- bits) value)
               (logior value (- (ash 1 bits)))
               value)))
    (with-pinned-context-code-object (context)
      (let* ((pc (context-pc context))
             (word (or instruction (sap-ref-32 pc 0)))
             (op (ldb (byte 2 30) word)))
        (cond
          ((= op 1)
           ;; call target: the word is the signed 30 bit word
           ;; displacement.
           (sap+ pc (* 4 (sx (ldb (byte 30 0) word) 30))))
          ((and (= op 2)
                (= (ldb (byte 6 19) word) 56)
                (logbitp 13 word))
           ;; jmpl: the target is rs1 + simm13.
           (int-sap
            (+ (context-register context (ldb (byte 5 14) word))
               (sx (ldb (byte 13 0) word) 13))))
          (t
           (error "Unrecognized call instruction: ~x, pc: ~x"
                  word pc)))))))


;;;; "Sigcontext" access functions, cut & pasted from alpha-vm.lisp.
;;;;
;;;; See also x86-vm for commentary on signed vs unsigned.

;;; This is like CONTEXT-REGISTER, but returns the value of a float
;;; register. FORMAT is the type of float to return.

(define-alien-routine ("os_context_float_register_addr" context-float-register-addr)
  (* long)
  (context (* os-context-t))
  (index int))

(defun context-float-register (context index format &optional integer)
  (declare (ignore integer)
           (ignorable context index))
  #+linux
  (let ((sap (alien-sap (context-float-register-addr context index))))
    (ecase format
      (single-float
       (coerce (sap-ref-double sap 0) 'single-float))
      (double-float
       (sap-ref-double sap 0))
      (complex-single-float
       (complex (coerce (sap-ref-double sap 0) 'single-float)
                (coerce (sap-ref-double sap 8) 'single-float)))
      (complex-double-float
       (complex (sap-ref-double sap 0)
                (sap-ref-double sap 8)))))
  #-linux
  (progn
    (warn "stub CONTEXT-FLOAT-REGISTER")
    (coerce 0 format)))

(defun %set-context-float-register (context index format value)
  (declare (type (alien (* os-context-t)) context))
  #+linux
  (let ((sap (alien-sap (context-float-register-addr context index))))
    (ecase format
      (single-float
       (setf (sap-ref-single sap 0) value))
      (double-float
       (setf (sap-ref-double sap 0) value))
      (complex-single-float
       (locally
           (declare (type (complex single-float) value))
         (setf (sap-ref-single sap 0) (realpart value)
               (sap-ref-single sap 4) (imagpart value))))
      (complex-double-float
       (locally
           (declare (type (complex double-float) value))
         (setf (sap-ref-double sap 0) (realpart value)
               (sap-ref-double sap 8) (imagpart value))))))
  #-linux
  (error "%set-context-float-register not working yet? ~S" (list context index format value)))

;;; Given a signal context, return the floating point modes word in
;;; the same format as returned by FLOATING-POINT-MODES.

;;; Under SunOS, we have a straightforward implementation in C:
#+sunos
(define-alien-routine ("os_context_fp_control" context-floating-point-modes)
    (unsigned 32)
  (context (* os-context-t)))

;;; Under Linux, we have to contend with utterly broken signal handling.
#+linux
(defun context-floating-point-modes (context)
  (declare (ignore context))
  (warn "stub CONTEXT-FLOATING-POINT-MODES")
  0)

(defun internal-error-args (context)
  (declare (type (alien (* os-context-t)) context))
  (/show0 "entering INTERNAL-ERROR-ARGS")
  (let* ((pc (context-pc context))
         (trap-number (sap-ref-8 pc 3)))
    (declare (type system-area-pointer pc))
    (sb-kernel::decode-internal-error-args (sap+ pc 4) trap-number)))

;; the sparc backend isn't important enough to deserve a fastrem-32 vop
(defun fastrem-32 (dividend c divisor)
  (declare (word dividend c divisor) (optimize (safety 0)))
  (multiple-value-bind (hi lo)
      (sb-bignum:%multiply (logand (* dividend c) most-positive-word) divisor)
    (declare (ignore lo))
    hi))
