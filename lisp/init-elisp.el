(add-hook 'emacs-lisp-mode-hook #'(lambda () (company-mode 1)))
(add-hook 'emacs-lisp-mode-hook #'(lambda () (setq-local indent-line-function 'elisp-indent-line)))

(setq lisp-body-indent 2)

(defun elisp-indent-line ()
  (interactive)
  (beginning-of-line)
  (delete-horizontal-space)
  (lisp-indent-line))

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
(define-key emacs-lisp-mode-map (kbd "<tab>") 'helm-yas-complete)
(define-key emacs-lisp-mode-map (kbd "M-.") 'elisp-find-function-under-point)

(provide 'init-elisp)
