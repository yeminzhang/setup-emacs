(add-hook 'emacs-lisp-mode-hook #'(lambda () (company-mode 1)))
(add-hook 'emacs-lisp-mode-hook #'(lambda () (setq-local indent-line-function 'elisp-indent-line)))
(add-hook 'emacs-lisp-mode-hook 'yas-minor-mode-on)

(setq lisp-body-indent 2)

(defun elisp-indent-line ()
  (interactive)
  (beginning-of-line)
  (delete-horizontal-space)
  (lisp-indent-line))

(defun elisp-find-function-under-point ()
  (interactive)
  (find-function (function-called-at-point)))

(defadvice eval-buffer (before save-buffer-before-eval activate)
  (save-buffer))

(provide 'init-elisp)
