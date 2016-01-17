#!/usr/bin/sbcl --noinform
;; If run from command line, the following if-test will succeed

;;(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
;;                                       (user-homedir-pathname))))
;;  (when (probe-file quicklisp-init)
;;    (load quicklisp-init)))

(defparameter db-file "~/english/books/full_english(utf-8).txt")

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))


(ql:quickload :cl-ppcre)
(defparameter result-ppcre (cl-ppcre:scan "(a)*b" "xaaabd"))

;;; data for dsl.
;;(defparameter data-dsl '(
;;                         ("word"
;;                          ("label" . " 



(format t "jopa!!!!!~d" result-ppcre)
;; (cl-ppcre:scan "(\\w+)\\W*\\[.*\\]" "abolitionist [ bə liʃ(ə)nist] n аболиционист")
;;


(defun get-border-entity (string)
  (format t "~d" (cl-ppcre:scan "(\\w+)\\W*\\[.*\\]" string))
  (print string)
  (print ""))

;; (get-border-entity "abolitionist [ bə liʃ(ə)nist] n аболиционист")

(defun next-entity (stream fn)
  (let ((current-string (read-line stream)))
    (funcall fn current-string)))

(with-open-file (stream db-file)
  (dotimes (x 100)
    (next-entity (stream #'get-border-entity))
                 
                  
    (format t "~a~% ~d" (read-line stream) (1+ x))))
;;(dotimes (x 20)
;;  (dotimes (y 20)
;;    (format t "~3d " (* (1+ x) (1+ y))))
;;  (format t "~%"))

;; (open "/some/file/name.txt" :direction :output :if-exists :supersede)


(if (> (length sb-ext:*posix-argv*) 1)
    ;;(format t "24jaoa")
    (format t  "~{ ~d~}" (list  2 3 4))
    ;; Code that is executed from the command line only goes here
)

