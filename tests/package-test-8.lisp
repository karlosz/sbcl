(defpackage "FOO" (:use :cl))

(in-package "FOO")

(defun f (x) x)

(setq *package* (find-package "CL-USER"))

(defun g (x) x)
