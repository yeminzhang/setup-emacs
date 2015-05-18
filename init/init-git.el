(require-packages '(magit))
(require 'magit)
;; reservered for future use when emacs 24.4 is ready
;;(require 'magit-filenotify)

(set-default 'magit-stage-all-confirm nil)
(set-default 'magit-unstage-all-confirm nil)

(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")

(add-to-list 'ido-ignore-buffers "\*magit")
;;(require 'magit-gerrit)

;; C-c C-a to amend without any prompt
(defun magit-just-amend ()
  (interactive)
  (save-window-excursion
    (shell-command "git --no-pager commit --amend --reuse-message=HEAD")
    (magit-refresh)))

(eval-after-load "magit"
  '(progn
     (define-key magit-status-mode-map (kbd "C-c C-a") 'magit-just-amend)
     (define-key magit-status-mode-map (kbd "C-c C-d") 'magit-diff-staged)
))

(eval-after-load 'info
  '(progn (info-initialize)
          (add-to-list 'Info-directory-list (expand-file-name "magit" user-emacs-directory))))

(global-set-key (kbd "<f4>")
  (lambda() (interactive)
    (magit-status (magit-get-top-dir) 'switch-to-buffer)))

;; restore magit-status when emacs starts
;; save magit-status buffer when save desktop
(defun magit-status-register-desktop-save ()
  "Set `desktop-save-buffer' to a function returning the dir of current repo."
  (setq desktop-save-buffer (lambda (desktop-dirname) (magit-get-top-dir))))

(add-hook 'magit-status-mode-hook 'magit-status-register-desktop-save)

(defun magit-status-restore-desktop-buffer (d-b-file-name d-b-name d-b-misc)
  "Restore a `magit-status' buffer on `desktop' load."
  (when (eq 'magit-status-mode desktop-buffer-major-mode)
    (let ((dir d-b-misc))
      (when dir
		(magit-status dir)
        (current-buffer)))))

(add-to-list 'desktop-buffer-mode-handlers '(magit-status-mode . magit-status-restore-desktop-buffer))

(provide 'init-git)
