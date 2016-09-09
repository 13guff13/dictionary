#!/usr/bin/sbcl --noinform
;;(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
;;                                       (user-homedir-pathname))))
;;  (when (probe-file quicklisp-init)
;;    (load quicklisp-init)))
;;
;;(ql:quickload :cl-ppcre)
(load "~/emacs/sbcl/dictionary/db-package.lisp")
(in-package :db-dictionary)
;;(princ (db-dictionary:search-in-db "home"))
;;(princ sb-ext:*posix-argv*)
(princ (db-dictionary:search-in-db-by-regexp (caddr sb-ext:*posix-argv*)))


;;(if (> (length sb-ext:*posix-argv*) 1)
;;    (princ sb-ext:*posix-argv*)

;;    (unless (null (cadr sb-ext:*posix-argv*))
;;      (load "/home/guff/emacs/sbcl/db-package.lisp")
;;      (in-package :db-dictionary)
;;      (princ (db-dictionary:search-in-db (cadr sb-ext:*posix-argv*))))

    ;; Code that is executed from the command line only goes here

;;)
