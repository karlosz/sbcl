;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;; we can only use this when the type system has been set up
(deftransform sb-pcl::pcl-instance-p ((object))
  (let* ((otype (lvar-type object))
         (standard-object (specifier-type 'standard-object)))
    (cond
      ;; Flush tests whose result is known at compile time.
      ((csubtypep otype standard-object) t)
      ((not (types-equal-or-intersect otype standard-object)) nil)
      (t
       `(sb-pcl::%pcl-instance-p object)))))
