(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

;; OS
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-mac* (eq system-type 'darwin))

;; set exec-path from $PATH env. $PATH env can be modified in custom.el
(setq exec-path (split-string (getenv "PATH") ":"))
(add-to-list 'exec-path exec-directory t)

(provide 'init-env)
