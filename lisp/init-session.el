(require-packages '(frame-cmds))

;; desktop save
(setq desktop-path (list user-emacs-directory))
(desktop-save-mode 1)
(setq desktop-restore-eager t)
;;(setq desktop-files-not-to-save "^$")

;;(require 'workgroups2)
;;(setq wg-mode-line-display-on nil
;;	  wg-modeline-string ""
;;	  wg-default-session-file (expand-file-name ".emacs_workgroups" user-emacs-directory))

;;(add-hook 'emacs-startup-hook 'wg-reload-session)
;;(add-hook 'kill-emacs-hook 'wg-save-session)
(require 'frame-cmds)
(global-set-key (kbd "C-M-o") 'other-frame)

(provide 'init-session)
