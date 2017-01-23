(use-package elisp-slime-nav
  :ensure t
  :diminish elisp-slime-nav-mode)

(use-package lisp-mode
  :defer t
  :config
  (setq lisp-body-indent 2)

  (defun elisp-indent-line ()
    (interactive)
    (beginning-of-line)
    (delete-horizontal-space)
    (lisp-indent-line))

  (defun elisp-save-and-eval-buffer ()
    (interactive)
    (save-buffer)
    (eval-buffer))

  (add-hook 'emacs-lisp-mode-hook #'(lambda () (company-mode 1)))
  (add-hook 'emacs-lisp-mode-hook #'(lambda () (setq-local indent-line-function 'elisp-indent-line)))
  (add-hook 'emacs-lisp-mode-hook 'yas-minor-mode-on)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode))

(provide 'init-elisp)
