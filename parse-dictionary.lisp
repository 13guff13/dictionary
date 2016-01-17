#!/usr/bin/sbcl --noinform
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :cl-ppcre)

(defparameter db-file "~/english/books/full_english(utf-8).txt")
(defparameter *words-list* ())
(defparameter *current-list* '("first" "first" "first" "first"))

(defun next-entity (stream fn)
  (let ((current-string (read-line stream)))
    (funcall fn current-string)))

(defun bind-to-global-variable (string)
  (multiple-value-bind (all-begin all-end sub-begin-vector sub-end-vector)
      (cl-ppcre:scan "\\W*([a-z,()1-9]+)\\W*\\[(.*)\\]" string)
    (cond ((and all-begin all-end sub-begin-vector sub-end-vector)
           (progn
             (push *current-list* *words-list*)
             (setq *current-list* (list
                    (subseq string (svref sub-begin-vector 0) (svref sub-end-vector 0))
                    (subseq string (svref sub-begin-vector 1) (svref sub-end-vector 1))
                    (subseq string all-end)))
             ))
          (T (setf (caddr *current-list*)
                   (concatenate 'string (caddr *current-list*) (format nil " ") string))))
    (list all-begin all-end sub-begin-vector sub-end-vector)))

(defmacro procesing-some-string (stream)
  `(with-open-file (stream ,stream)
     (print "waitign")
     (dotimes (x 727)
       (read-line stream)
       (format t ".")) 
     (dotimes (x 164176)
;;     (dotimes (x 10)
       (next-entity stream #'bind-to-global-variable))))
(procesing-some-string db-file)
(princ *words-list*)

(with-open-file (stream "test.lisp" :direction :output
                        :if-does-not-exist :create
                        :if-exists :overwrite)
  
  (format stream "(defparameter words-db '(")
  (write *words-list* :stream stream :escape t)
;;  (dolist (word *words-list*)
;;    (format stream "\(~{~(\(\"~a\"\)~) ~}\)" word))
  (format stream "))"))

(load "test.lisp")



;;(with-open-file (stream "test.txt" :direction :output
;;                        :if-does-not-exist :create
;;                        :if-exists :overwrite)
;;  (princ *words-list* stream))
;;(defun read-whole-file (filename)
;;  (with-open-file (in filename)
;;    (let ((buffer (make-string (file-length in))))
;;      (read-sequence buffer in)
;;      buffer)))

;;(procesing-some-string db-file)
;;(defparameter test-string  " admission [əd miʃ(ə)n] n 1. доступ; вход; ∼ fee вступительный взнос; входная плата; ∼ by ticket вход по")
;;(bind-to-global-variable test-string)

;;(defparameter result-ppcre (cl-ppcre:scan "(a)*b" "xaaabd"))

;;; data for dsl.
;;(defparameter data-dsl '(
;;                         ("word"
;;                          ("label" . " 



;;(format t "jopa!!!!!~d" result-ppcre)
;; (cl-ppcre:scan "(\\w+)\\W*\\[.*\\]" "abolitionist [ bə liʃ(ə)nist] n аболиционист")
;;

;;(defparameter a 2)
;;(defun a ()
;;  (defparameter a 1)
;;  (b))
;;(defun b () (setq a 3))



;;(defun get-border-entity (string)
;;  (format t "~d in string: ~a~%" (cl-ppcre:scan "(\\w+)\\W*\\[.*\\]" string) string)
;;  )


;; (get-border-entity "abolitionist [ bə liʃ(ə)nist] n аболиционист")

    
;;(dotimes (x 20)
;;  (dotimes (y 20)
;;    (format t "~3d " (* (1+ x) (1+ y))))
;;  (format t "~%"))

;; (open "/some/file/name.txt" :direction :output :if-exists :supersede)
