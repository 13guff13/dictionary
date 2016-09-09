;;;; That is a wrapper file used in `lc' script when compiler build image for
;;;; file(s? at this time - just for one file that can require another files/systems).

;;;; It also possible to rebuild this program using this program again
;;;; (and don't use the shell script).

(defpackage   #:lc-user
  (:use       #:common-lisp
              #:sb-sys
              #:sb-ext)
  (:export    #:continue-if-only
              #:with-quit
              #:global-speed-extreme-optimize
              #:main
              #:@main
              #:program-name))

(in-package #:lc-user)

;;;; stuff that can be used in compiled file

(defmacro continue-if-only (thing error-string &rest error-arguments)
  `(unless ,thing
     (format *stderr* ,error-string ,@error-arguments)
     (exit)))

(defmacro with-quit (&body body)
  `(unwind-protect
       (progn ,@body)
     (exit)))

(defmacro global-speed-extreme-optimize (&optional (speed 3))
  `(proclaim '(optimize (speed ,speed)
                        (safety 0)
                        (debug 0)
                        (compilation-speed 0)
                        (space 0))))

;;;; default enrty point

(defmacro main (arguments early-forms &body body)
  `(defun @main ()
     ,@early-forms
     (destructuring-bind (program-name ,@arguments)
                         (mapcar #'read-from-string *posix-argv*)
       (declare (ignorable program-name))
       ,@body)))

;;;; compiling

(defparameter *usage-string*
  "Usage: lc [file] [:compile] [:load] [:gc] [:image].~%")

(let ((file (second *posix-argv*))
      (options (rest (rest *posix-argv*))))
  (continue-if-only file *usage-string*)
  (continue-if-only (probe-file file) "No such file ~A~%" file)
  (let (load-or-compile-p)
    (loop :for option :in options
          :do (ecase (read-from-string option)
                (:compile  (compile-file file)
                           (setf load-or-compile-p t))
                (:load     (load file)
                           (setf load-or-compile-p t))
                (:gc       (gc :full t))
                (:image    (continue-if-only (fboundp 'main)
                             "SUBJECT file must contain the MAIN function.~%")
                           (save-lisp-and-die (format nil "~A.bin" file)
                                              :purify t
                                              :executable t
                                              :toplevel '@main))))
    (continue-if-only load-or-compile-p *usage-string*)))
