;;;; X86-specific runtime stuff

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMUCL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defun machine-type ()
  "Return a string describing the type of the local machine."
  "X86")

;;;; low-level signal context access functions
;;;;
;;;; Note: In CMUCL, similar functions were hardwired to access
;;;; BSD-style sigcontext structures defined as alien objects. Our
;;;; approach is different in two ways:
;;;;   1. We use POSIX SA_SIGACTION-style signals, so our context is
;;;;      whatever the void pointer in the sigaction handler dereferences
;;;;      to, not necessarily a sigcontext.
;;;;   2. We don't try to maintain alien definitions of the context
;;;;      structure at Lisp level, but instead call alien C functions
;;;;      which take care of access for us. (Since the C functions can
;;;;      be defined in terms of system standard header files, they
;;;;      should be easier to maintain; and since Lisp code uses signal
;;;;      contexts only in interactive or exception code (like the debugger
;;;;      and internal error handling) the extra runtime cost should be
;;;;      negligible.

(defun return-machine-address (context)
  (let ((sp (context-register context sb-vm::esp-offset)))
    (sap-ref-word (int-sap sp) return-pc-save-offset)))

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
             (read-byte
               (lambda (i)
                 (if (and instruction (< i 4))
                     (ldb (byte 8 (* i 8)) instruction)
                     (sap-ref-8 pc i))))
             (read-dword
               (lambda (i)
                 (+ (funcall read-byte i)
                    (ash (funcall read-byte (+ i 1)) 8)
                    (ash (funcall read-byte (+ i 2)) 16)
                    (ash (funcall read-byte (+ i 3)) 24))))
             (opcode (funcall read-byte 0))
             (modrm (funcall read-byte 1)))
        (cond
          ((= opcode #xE8)
           ;; CALL rel32.
           (sap+ pc (+ 5 (sx (funcall read-dword 1) 32))))
          ((and (= opcode #xFF)
                (= (ldb (byte 3 3) modrm) 2))
           ;; CALL r/m32: FF /2.  A memory operand dereferences to
           ;; the raw entry point of the callee; a register operand
           ;; holds it.
           (int-sap
            (let* ((mod (ldb (byte 2 6) modrm))
                   (rm (ldb (byte 3 0) modrm))
                   (has-sib (and (/= mod 3) (= rm 4)))
                   (sib (if has-sib (funcall read-byte 2) 0))
                   (modrm-end (if has-sib 3 2))
                   (disp
                    (cond ((= mod 1)
                           (sx (funcall read-byte modrm-end) 8))
                          ((or (= mod 2)
                               (and has-sib
                                    (= (ldb (byte 3 0) sib) 5))
                               (= rm 5))
                           (sx (funcall read-dword modrm-end) 32))
                          (t 0))))
              (if (= mod 3)
                  (context-register context rm)
                  (+ (cond ((and (= mod 0) (= rm 5))
                            ;; absolute, no base
                            0)
                           ((and has-sib (= mod 0)
                                 (= (ldb (byte 3 0) sib) 5))
                            0)
                           (t (context-register
                               context
                               (if has-sib
                                   (ldb (byte 3 0) sib)
                                   rm))))
                     (if (and has-sib
                              (/= (ldb (byte 3 3) sib) 4))
                         (ash (context-register
                               context (ldb (byte 3 3) sib))
                              (ldb (byte 2 6) sib))
                         0)
                     disp)))))
          (t
           (error "Unrecognized call instruction: ~x, pc: ~x"
                  opcode pc)))))))

#+(or linux win32)
(define-alien-routine ("os_context_float_register_addr" context-float-register-addr)
  (* unsigned) (context (* os-context-t)) (index int))

(defun context-float-register (context index format &optional integer)
  (declare (ignore integer)
           (ignorable context index))
  #-(or linux win32)
  (progn
    (warn "stub CONTEXT-FLOAT-REGISTER")
    (coerce 0 format))
  #+(or linux win32)
  (let ((sap (alien-sap (context-float-register-addr context index))))
    (ecase format
      (single-float
       (coerce (sap-ref-long sap 0) 'single-float))
      (double-float
       (sap-ref-long sap 0))
      (complex-single-float
       (complex (coerce (sap-ref-long sap 0) 'single-float)
                (coerce (sap-ref-long sap 10) 'single-float)))
      (complex-double-float
       (complex (sap-ref-long sap 0)
                (sap-ref-long sap 10))))))

(defun %set-context-float-register (context index format new-value)
  (declare (ignore context index))
  (warn "stub %SET-CONTEXT-FLOAT-REGISTER")
  (coerce new-value format))

;;; Given a signal context, return the floating point modes word in
;;; the same format as returned by FLOATING-POINT-MODES.
#-(or linux sunos)
(defun context-floating-point-modes (context)
  ;; FIXME: As of sbcl-0.6.7 and the big rewrite of signal handling for
  ;; POSIXness and (at the Lisp level) opaque signal contexts,
  ;; this is stubified. It needs to be rewritten as an
  ;; alien function.
  (declare (ignore context)) ; stub!
  (warn "stub CONTEXT-FLOATING-POINT-MODES")
  0)

#+(or linux sunos)
(define-alien-routine ("os_context_fp_control" context-floating-point-modes)
    (sb-alien:unsigned 32)
  (context (* os-context-t)))

;;;; INTERNAL-ERROR-ARGS

;;; Given a (POSIX) signal context, extract the internal error
;;; arguments from the instruction stream.
(defun internal-error-args (context)
  (declare (type (alien (* os-context-t)) context))
  (let* ((pc (context-pc context))
         (trap-number (sap-ref-8 pc 0)))
    (declare (type system-area-pointer pc))
    (sb-kernel::decode-internal-error-args (sap+ pc 1) trap-number)))

;;; This is used in error.lisp to insure that floating-point exceptions
;;; are properly trapped. The compiler translates this to a VOP.
(defun float-wait ()
  (float-wait))

;;; float constants
;;;
;;; These are used by the FP MOVE-FROM-{SINGLE|DOUBLE} VOPs rather
;;; than the i387 load constant instructions to avoid consing in some
;;; cases. Note these are initialized by GENESIS as they are needed
;;; early.
(defvar *fp-constant-0f0*)
(defvar *fp-constant-1f0*)
(defvar *fp-constant-0d0*)
(defvar *fp-constant-1d0*)
;;; the long-float constants
(defvar *fp-constant-0l0*)
(defvar *fp-constant-1l0*)
(defvar *fp-constant-pi*)
(defvar *fp-constant-l2t*)
(defvar *fp-constant-l2e*)
(defvar *fp-constant-lg2*)
(defvar *fp-constant-ln2*)

;;; Support for the MT19937 random number generator. The update
;;; function is implemented as an assembly routine. This definition is
;;; transformed to a call to the assembly routine allowing its use in
;;; interpreted code.
(defun random-mt19937 (state)
  (declare (type (simple-array (unsigned-byte 32) (627)) state))
  (random-mt19937 state))
