(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

;; OS
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-mac* (eq system-type 'darwin))

(provide 'init-env)
