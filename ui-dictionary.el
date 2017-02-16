(defun translate-at-point ()
  (interactive)
  (if (setq bounds (bounds-of-thing-at-point 'word))
      (translation-buffer (buffer-substring-no-properties
			   (car bounds)
			   (cdr bounds)))
    (message "don't find any word.")))
;;(translate-at-point)

(defun translation-buffer (content)
  (let ((buffer-name "*translation-buffer*")
	(command (concatenate 'string "sbcl ~/emacs/external-git-repo/dictionary/dictionary-cli2.lisp " content)))
    (with-output-to-temp-buffer buffer-name
      (start-process-shell-command "execute-process" buffer-name command))
    (select-window (display-buffer buffer-name))
    (set-buffer buffer-name)
    (insert content)))
(local-set-key (kbd "C-c C-t") 'translate-at-point)
;;(translation-buffer "jopa")




(setq buffer (get-buffer-create "*test-sbcl*"))

(setq process (start-process "test process sbcl" buffer "sbcl"))
(select-window (display-buffer buffer))

(kill-process process)
(process-command process)
(process-status process)
(process-filter process)

(process-send-string process "(+ 2 3 12)\n")
(process-send-string process "(load \"~/emacs/external-git-repo/dictionary/dictionary-init.lisp\")\n")
(process-send-string process "(db-dictionary:search-in-db-by-regexp \"jopa\")\n")

;; (defun slime-output-filter (process string)
;;   (with-current-buffer (process-buffer process)
;;     (when (and (plusp (length string))
;;                (eq (process-status slime-buffer-connection) 'open))
;;       (slime-write-string string))))


(setq kept nil)

(defun my-output-filter (process string)
  (with-current-buffer (process-buffer process)
    (setf kept (cons string kept))
    (insert string)))

(set-process-filter process 'my-output-filter)

;; notice doesn't work.
;; (add-function :before (process-filter process) 'my-output-filter)

(setq translate-output-buffer nil)
(get-buffer-create "*translate-output*")

;; run sequence.
(defun run-translation-process()
  (setq history-of-translation-list nil)
  (setq buffer (get-buffer-create "*test-sbcl*"))
  (defun get-translate-output-buffer()
    (if (get-buffer "*translate-output*")
	(get-buffer "*translate-output*")
      (get-buffer-create "*translate-output*")))
  (defun my-output-filter(process string)
    (with-current-buffer (process-buffer process)
      (setf history-of-translation-list (cons string history-of-translation-list))
      (with-current-buffer (get-translate-output-buffer)
	(insert string))
      (insert string)))
  (defun translate-word-trigger(word)
    (process-send-string process (concatenate 'string "(db-dictionary:search-in-db-by-regexp \"" word "\")\n")))
  (defun translate-at-point()
    (interactive)
    (if (setq bounds (bounds-of-thing-at-point 'word))
	(progn
	  ;; todo added scroll cursor to the top of translate-output-buffer buffer.
	  (with-current-buffer (get-translate-output-buffer)
	    (goto-char (point-min)))
	  (display-buffer (get-translate-output-buffer))
	  (translate-word-trigger (buffer-substring-no-properties
				   (car bounds)
				   (cdr bounds))))
      (message "don't find any word.")))
  (local-set-key (kbd "C-t") 'translate-at-point)
  (setq process (start-process "sbcl-for-translating-script" buffer "sbcl"))
  (process-send-string process "(load \"~/emacs/external-git-repo/dictionary/dictionary-init.lisp\")\n")
  (set-process-filter process 'my-output-filter)
  (message "process translation has starded"))
;; end run sequence.

(run-translation-process)

(scroll-down 100000)
