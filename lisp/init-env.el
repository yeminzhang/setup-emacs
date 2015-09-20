(defcustom *is-sv-kbmap* nil
  "Current keyboard layout."
  :type 'boolean)

(defun env-set-keyboard-layout()
  (interactive)
  (if (y-or-n-p "Do you use Swedish Keyboard layout? ")
      (customize-save-variable '*is-sv-kbmap* t)
    (customize-save-variable '*is-sv-kbmap* nil))
  (message "Please restart emacs to make it into effect"))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

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

(defun check-executable (executable-name warning-msg)
  (unless (executable-find executable-name)
	(warn warning-msg)))

(defun env-check()
  (interactive)
  (check-executable "updatedb" "You don't have updatedb installed. locate command will not work")
  (check-executable "locate" "You don't have locate installed. locate command will not work")
  (check-executable "find" "You don't have find installed. Search of directories using helm will not work")
  (check-executable "gs" "You don't have ghostscript(gs) installed. pdf viewer(doc-view) will not work")
  (check-executable "scp" "You don't have scp installed. Remote copy under dired-mode will not work")
  (check-executable "ssh" "You don't have ssh installed. ssh to other nodes will not work")
  (check-executable "sshpass" "You don't have sshpass installed. non-interactive ssh will not work")
  (check-executable "readelf" "You don't have readelf installed. readelf of executable  will not work")
  (check-executable "gdb" "You don't have gdb installed. Debug using gdb will not work")
  (check-executable "git" "You don't have git installed. magit mode will not work")
  (check-executable "clang-format" "clang-format is NOT found under exec-path. code format will not work")
  )

(defun display-warning-buffer ()
  (let (
		(buffer (get-buffer "*Warnings*"))
		)
	(when buffer (switch-to-buffer buffer))))

(add-hook 'after-init-hook 'display-warning-buffer)

(env-check)
(provide 'init-env)
