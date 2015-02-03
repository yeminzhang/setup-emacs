(defun print-time ()
  (interactive)
  (message (current-time-string)))

(global-set-key (kbd "<f1>") 'print-time)

(provide 'init-misc)
