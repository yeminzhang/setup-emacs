(load (expand-file-name "env-default.el" user-emacs-directory))
(if (file-exists-p (expand-file-name "env.el" user-emacs-directory))
	(load (expand-file-name "env.el" user-emacs-directory)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
	(load custom-file))

;; set right-little-finger-key based on keyboard layout
(if (and (boundp 'keyboard-layout) (string= keyboard-layout "sv"))
    (defconst right-little-finger-key "ö")
  (defconst right-little-finger-key ";"))

;; OS
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-mac* (eq system-type 'darwin))

(provide 'init-env)

