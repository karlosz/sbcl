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

(defun interned-symbol-p (x) (and (symbolp x) (symbol-package x)))

(flet ((struct-accessor-p (object slot-name)
         (let ((c-slot-name (lvar-value slot-name)))
           (unless (interned-symbol-p c-slot-name)
             (give-up-ir1-transform "slot name is not an interned symbol"))
           (let* ((type (lvar-type object))
                  (dd (when (structure-classoid-p type)
                        (find-defstruct-description
                         (sb-kernel::structure-classoid-name type)))))
              (when dd
                (find c-slot-name (dd-slots dd) :key #'dsd-name))))))

  (deftransform slot-boundp ((object slot-name) (t (constant-arg symbol)) *
                             :node node)
    (cond ((struct-accessor-p object slot-name) t) ; always boundp
          (t (delay-ir1-transform node :constraint)
             `(sb-pcl::accessor-slot-boundp object ',(lvar-value slot-name)))))

  (deftransform slot-value ((object slot-name) (t (constant-arg symbol)) *
                            :node node)
    (acond ((struct-accessor-p object slot-name)
            `(,(dsd-accessor-name it) object))
           (t
            (delay-ir1-transform node :constraint)
            `(sb-pcl::accessor-slot-value object ',(lvar-value slot-name)))))

  (deftransform sb-pcl::set-slot-value ((object slot-name new-value)
                                        (t (constant-arg symbol) t)
                                        * :node node)
    (acond ((struct-accessor-p object slot-name)
            `(setf (,(dsd-accessor-name it) object) new-value))
           ((policy node (= safety 3))
            ;; Safe code wants to check the type, and the global
            ;; accessor won't do that.
            (give-up-ir1-transform "cannot use optimized accessor in safe code"))
           (t
            (delay-ir1-transform node :constraint)
            `(sb-pcl::accessor-set-slot-value object ',(lvar-value slot-name)
                                              new-value)))))
