(package-install-when-not-exist '(magit))
(require 'magit)
;; reservered for future use when emacs 24.4 is ready
;;(require 'magit-filenotify)

(set-default 'magit-stage-all-confirm nil)
(set-default 'magit-unstage-all-confirm nil)

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
    (magit-status (magit-get-top-dir))))

(provide 'init-magit)
