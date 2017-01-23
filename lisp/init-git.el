(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-status-buffer-switch-function 'switch-to-buffer
        magit-push-always-verify nil)
  (set-display-buffer-other-window (rx bos "*magit-rev:"))
  (set-display-buffer-other-window (rx bos "*magit-diff:"))
  (set-display-buffer-other-window (rx bos "*magit-log:"))
  (set-display-buffer-other-window (rx bos "*magit:")))

(use-package magit-status
  :defer t
  :config
  ;; C-c C-a to amend without any prompt
  (defun magit-just-amend ()
    (interactive)
    (save-window-excursion
      (shell-command "git --no-pager commit --amend --reuse-message=HEAD")
      (magit-refresh)))
  )

(use-package info
  :defer t
  :config
  (info-initialize)
  (add-to-list 'Info-directory-list (expand-file-name "magit" user-emacs-directory)))

(provide 'init-git)
