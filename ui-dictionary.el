(defun translate-at-point ()
  (interactive)
  (if (setq bounds (bounds-of-thing-at-point 'word))
      (translation-buffer (buffer-substring-no-properties
                       (car bounds)
                       (cdr bounds)))
      (message "don't find any word.")))
;;(translate-at-point)

(defun translation-buffer (content)
  (let ((buffer-name "*translate dictionary*"))
    (with-output-to-temp-buffer buffer-name
      (select-window (display-buffer buffer-name))
      (set-buffer buffer-name)
      (insert content))))

;;(translation-buffer "jopa")

(defun translate-at-point2 ()
  (interactive)
  (if (setq bounds (bounds-of-thing-at-point 'word))
      (translation-buffer2 (buffer-substring-no-properties
                       (car bounds)
                       (cdr bounds)))
      (message "don't find any word.")))


(defun translation-buffer2 (str)
  (let ((buffer-name "*translate dictionary*"))
    (with-output-to-temp-buffer buffer-name
      (select-window (display-buffer buffer-name))
      (set-buffer buffer-name)
      (set-process-sentinel
       (start-file-process "jopa" buffer-name "~/emacs/sbcl/dictionary/dictionary-cli2.lisp" str)
       (lambda (process event)
         (message "I've done."))))))

home
(translation-buffer2 "home")

 
