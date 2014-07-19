(define-key emacs-lisp-mode-map (kbd "<f1>") 'save-and-eval-buffer)

(add-hook 'emacs-lisp-mode-hook #'(lambda () (company-mode 1)))

(defun elisp-find-function-under-point ()
(interactive)
(find-function (function-called-at-point)))

(defun save-and-eval-buffer ()
(interactive)
(progn
(save-buffer)
(eval-buffer)
))

(provide 'init-elisp)
