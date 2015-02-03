(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(dolist (pkg '(smex company dired+ helm helm-gtags magit undo-tree zenburn-theme volatile-highlights function-args projectile helm-projectile))
(unless (package-installed-p pkg) (package-install pkg)))

(setq emacs-configuration-root-dir "~/.emacs.d/")
(load (concat emacs-configuration-root-dir "env.el"))

;; set right-little-finger-key based on keyboard layout
(if (and (boundp 'keyboard-layout) (string= keyboard-layout "sv"))
    (setq right-little-finger-key "ö")
  (setq right-little-finger-key ";"))

(add-to-list 'load-path (concat emacs-configuration-root-dir "init"))
(require 'init-helm)
(require 'init-company)
(require 'init-edit)
(require 'init-buffer)
(require 'init-view)
(require 'init-term)
(require 'init-eshell)
(require 'init-dired)
(require 'init-remote)
(require 'init-project)
(require 'init-elisp)
(require 'init-c)
(if (and (boundp 'erlang-emacs-tools-dir) (file-exists-p erlang-emacs-tools-dir))
	(require 'init-erlang))
(require 'init-magit)
(require 'init-misc)
(require 'init-appearance)

;; desktop save
(desktop-save-mode 1)
(setq desktop-restore-eager t)
(setq desktop-files-not-to-save "^$")

;;(add-to-list 'load-path (concat emacs-configuration-root-dir "workgroups2/src"))
;;(require 'workgroups2)
;;(setq wg-mode-line-disable t)
;;(setq wg-modeline-string "")
;;(workgroups-mode 1)

;;(add-hook 'emacs-startup-hook #'(lambda () (wg-reload-session)))

