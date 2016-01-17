(defpackage :db-dictionary
  (:use :common-lisp)
  (:export :search-in-db
           :update-dictionary
           :store))
(in-package :db-dictionary)

(defparameter db-file-path "~/emacs/sbcl/test.lisp")
(defparameter words-db ())
(load db-file-path)

(defun update-dictionary ()
  (with-open-file (stream db-file-path :direction :output
                          :if-does-not-exist :create
                          :if-exists :overwrite)
    (format stream "(defparameter words-db '(")
    (write words-db :stream stream :escape t)
    (format stream "))")))

(defun search-in-db (str)
  (dolist (n words-db)
      (cond ((string-equal (car n) str)
             (return n)))))


  

  

