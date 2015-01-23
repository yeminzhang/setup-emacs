(add-hook 'emacs-lisp-mode-hook #'(lambda () (company-mode 1)))

(defun elisp-find-function-under-point ()
(interactive)
(find-function (function-called-at-point)))

(defun elisp-save-and-eval-buffer ()
(interactive)
(progn
(save-buffer)
(eval-buffer)
))

(define-key emacs-lisp-mode-map (kbd "<f6>") 'elisp-save-and-eval-buffer)

(provide 'init-elisp)
