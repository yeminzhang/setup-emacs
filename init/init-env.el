(require 'tramp)

;; default value
(setq *is-sv-kbmap* t)
(setq ssh-tunnel-host-list '())

(defun env-customize ()
  (interactive)
  (env-set-keyboard-layout)
  (env-set-ssh-tunnel-list))

(defun env-set-keyboard-layout()
  (interactive)
  (if (y-or-n-p "Do you use Swedish Keyboard layout? ")
    (customize-save-variable '*is-sv-kbmap* t)
    (customize-save-variable '*is-sv-kbmap* nil)))

(defun env-set-ssh-tunnel-list()
  (interactive)
  (let (
	(sconfig-list (tramp-parse-sconfig "~/.ssh/config"))
	(ssh-tunnel-list ()))
    (dolist (login sconfig-list)
      (if (and login (nth 1 login) (y-or-n-p (concat "Add " (nth 1 login) " to ssh tunnel list? ")))
		  (add-to-list 'ssh-tunnel-list (nth 1 login) t)))
    (customize-save-variable 'ssh-tunnel-host-list ssh-tunnel-list)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (env-customize))

(load custom-file)

;; set right-little-finger-key based on keyboard layout
(if *is-sv-kbmap*
	(defconst right-little-finger-key "ö")
  (defconst right-little-finger-key ";"))
(if *is-sv-kbmap*
	(defconst bottom-right-finger-key "-")
  (defconst bottom-right-finger-key "/"))

;; OS
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-mac* (eq system-type 'darwin))

(provide 'init-env)

