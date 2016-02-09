(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :cl-ppcre)
(defpackage :db-dictionary
  (:use :common-lisp)
;;        :cl-ppcre)
  (:export :search-in-db
           :search-in-db-by-regexp
           :update-dictionary
           :store))

;;      (ql:quickload :cl-ppcre)
;;      (cl-ppcre:scan "\\W*([a-z,()1-9]+)\\W*\\[(.*)\\]" string)


(in-package :db-dictionary)

(defparameter db-file-path "./test.lisp")
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

(defun extract-value (item str)
  (multiple-value-bind (all-begin)  
      (cl-ppcre:scan (concatenate 'string (format nil ".*(")
                                  str (format nil ").*"))
                     (car item))
    (if (null all-begin) nil
        item)))
;;(funcall  #'extract-value (cons " ssdfsdhome 234" (cons "home is home!" nil)) "home")


  
(defun collect-elements (count predicate list-db str-pattern)
  (do ((current-item (car list-db) (car (setq list-db (cdr list-db)))) (collect-result-list nil))
      ((or (null current-item) (> (length collect-result-list) count))
       collect-result-list)
    (progn
      (cond ((funcall predicate current-item str-pattern)
             (push current-item collect-result-list)))
      )))


;;(defparameter a (collect-elements 1 #'extract-value
;;                     '(("home" "sfsdfsdf1")("homeless" "hosdf2"))
;;                     "home"))




(defun search-in-db-by-regexp (pattern)
  (collect-elements 3
                    #'extract-value
                    words-db
                    pattern))

  

  

