(require-packages '(magit))

;; C-c C-a to amend without any prompt
(defun magit-just-amend ()
  (interactive)
  (save-window-excursion
    (shell-command "git --no-pager commit --amend --reuse-message=HEAD")
    (magit-refresh)))

(after-load 'magit
  (define-key magit-status-mode-map (kbd "C-c C-a") 'magit-just-amend)
  (define-key magit-log-mode-map (kbd "g") 'beginning-of-buffer)
  (define-key magit-log-mode-map (kbd "G") 'end-of-buffer)
  (setq magit-status-buffer-switch-function 'switch-to-buffer
        magit-push-always-verify nil
        auto-revert-check-vc-info t)
  (add-hook 'magit-status-mode-hook 'magit-status-register-desktop-save)
  (set-display-buffer-other-window (rx bos "*magit-rev:"))
  (set-display-buffer-other-window (rx bos "*magit-diff:"))
  (set-display-buffer-other-window (rx bos "*magit-log:"))
  (set-display-buffer-other-window (rx bos "*magit:"))
  )

(after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list (expand-file-name "magit" user-emacs-directory)))

(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g l") (lambda () (interactive) (magit-log '("HEAD"))))
(global-set-key (kbd "C-c g d") (lambda () (interactive) (call-interactively 'magit-diff-popup)))
(global-set-key (kbd "C-c g b") (lambda () (interactive) (call-interactively 'magit-branch-popup)))
(global-set-key (kbd "C-c g P") 'magit-pull-popup)

;; restore magit-status when emacs starts
;; save magit-status buffer when save desktop
(defun magit-status-register-desktop-save ()
  "Set `desktop-save-buffer' to a function returning the dir of current repo."
  (setq desktop-save-buffer (lambda (desktop-dirname) (magit-toplevel))))

(defun magit-status-restore-desktop-buffer (d-b-file-name d-b-name d-b-misc)
  "Restore a `magit-status' buffer on `desktop' load."
  (when (eq 'magit-status-mode desktop-buffer-major-mode)
    (let ((dir d-b-misc))
      (when dir
        (magit-status dir)
        (current-buffer)))))

(after-load 'desktop
  (add-to-list 'desktop-buffer-mode-handlers '(magit-status-mode . magit-status-restore-desktop-buffer)))

(provide 'init-git)
