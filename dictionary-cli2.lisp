(load "~/emacs/sbcl/dictionary/db-package.lisp")

(main (string)
      ()
      (let ((pattern (string string)))
        (format t "~a" (db-dictionary:search-in-db-by-regexp pattern))))
