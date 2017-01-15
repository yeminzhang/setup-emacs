(require-packages '(elisp-slime-nav))

(add-hook 'emacs-lisp-mode-hook #'(lambda () (company-mode 1)))
(add-hook 'emacs-lisp-mode-hook #'(lambda () (setq-local indent-line-function 'elisp-indent-line)))
(add-hook 'emacs-lisp-mode-hook 'yas-minor-mode-on)
(add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)

(after-load 'elisp-slime-nav
  (diminish 'elisp-slime-nav-mode))

(setq lisp-body-indent 2)

(defun elisp-indent-line ()
  (interactive)
  (beginning-of-line)
  (delete-horizontal-space)
  (lisp-indent-line))

(defadvice eval-buffer (before save-buffer-before-eval activate)
  (save-buffer))

(provide 'init-elisp)
