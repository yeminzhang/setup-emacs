(defun print-time ()
  (interactive)
  (message (current-time-string)))

(defun file-type (filename)
  (interactive (list (ido-read-file-name "file: ")))
	  (shell-command (concat "file " filename)))

(global-set-key (kbd "<f1>") 'print-time)

(provide 'init-misc)
