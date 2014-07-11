(define-key emacs-lisp-mode-map (kbd "<f1>") 'save-and-eval-buffer)

(defun save-and-eval-buffer ()
(interactive ())
(save-buffer)
(eval-buffer)
)

(provide 'init-elisp)
