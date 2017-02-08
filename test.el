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

(local-set-key (kbd "C-c t") 'translate-at-point)
;;(translation-buffer "jopa")
