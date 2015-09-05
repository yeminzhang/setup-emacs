(require-packages '(helm-proc))

(defun print-time ()
  (interactive)
  (message (current-time-string)))

(defun file-type (filename)
  (interactive (list (ido-read-file-name "file: ")))
  (shell-command (concat "file " filename)))

(global-set-key (kbd "<f1>") 'helm-proc)

(provide 'init-misc)
