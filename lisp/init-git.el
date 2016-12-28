(require-packages '(magit))

;; C-c C-a to amend without any prompt
(defun magit-just-amend ()
  (interactive)
  (save-window-excursion
    (shell-command "git --no-pager commit --amend --reuse-message=HEAD")
    (magit-refresh)))

(after-load 'magit
  (setq magit-status-buffer-switch-function 'switch-to-buffer
        magit-push-always-verify nil
        auto-revert-check-vc-info t)
  (add-hook 'magit-status-mode-hook 'magit-status-register-desktop-save)
  (set-display-buffer-other-window (rx bos "*magit-rev:"))
  (set-display-buffer-other-window (rx bos "*magit-diff:"))
  (set-display-buffer-other-window (rx bos "*magit-log:"))
  (set-display-buffer-other-window (rx bos "*magit:")))

(after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list (expand-file-name "magit" user-emacs-directory)))

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
