(require-packages '(function-args company-c-headers))
;;(require 'flymake-cursor)
;;(add-hook 'find-file-hook 'flymake-find-file-hook)

(defun flymake-simple-make-init ()
(list "make"
(list "-s" "-C" "./")))

(setq gdb-speedbar-auto-raise t)

(defun cc-debug (executable parameters envs)
  (interactive
	  (list (ido-read-file-name "Executable: ")
			(read-from-minibuffer "Parameters: ")
			(read-from-minibuffer "Envs: ")
			))
  (setq command-line (concat "gdb -i=mi " executable)
		gdb-many-windows t
		gdb-show-main t)
  (gdb command-line)
  (setq-local project-executable-file executable)
  (if parameters (gud-send-command (concat "set args " parameters)))
  (if envs (let
			 ((env-list (split-string envs ",")))
			 (dolist (env env-list)
			   (gud-send-command (concat "set environment " env)))))
  (gdb-clear-inferior-io)
  (if (file-exists-p (cc-breakpoints-filename))
	  (progn
		(gud-send-command (concat "source " (cc-breakpoints-filename)))
		(gud-run 0)
		))
  )

(defun cc-breakpoints-filename ()
  (with-current-buffer "*gud-main*"
  (concat "/tmp/" (s-replace "/" "!" project-executable-file))))

(defun cc-debug-quit ()
  (interactive ())
  (gud-send-command (concat "save breakpoints " (cc-breakpoints-filename)))
  (gud-send-command "quit")
  )

(defun cc-get-alternative-filename ()
  (let
	  (
	   (bse (file-name-nondirectory (file-name-sans-extension buffer-file-name)))
	   (ext (downcase (file-name-extension buffer-file-name))))
	(cond
	 ((equal ext "h")
	  (concat bse ".c"))
	 ((equal ext "c")
	  (concat bse ".h"))
	 ((or (equal ext "cpp") (equal ext "cc"))
	  (concat bse ".hh"))
	 ((equal ext "hh")
	  (concat bse ".cc"))
	 )))

(defun cc-switch-source-header-file ()
  (interactive)
  (helm-gtags-find-files (cc-get-alternative-filename)))

(defun cc-get-headers-path-system ()
  (if (and (projectile-project-p) (eq 'c/c++ projectile-project-type) (ede-cpp-root-load (projectile-project-root)))
	  (oref (ede-cpp-root-load (projectile-project-root)) :system-include-path)
	(list "/usr/include" "/usr/local/include" "/usr/include/c++/4.8")
	))

(defun cc-get-headers-path-user ()
  (if (and (projectile-project-p) (eq 'c/c++ projectile-project-type) (ede-cpp-root-load (projectile-project-root)))
	  (let*
		  ((ede-cpp-project (ede-cpp-root-load (projectile-project-root)))
		   (ede-include-path-user (oref ede-cpp-project :include-path))
		   (company-include-path-user '("." "..")))
		(dolist (ede-include-path ede-include-path-user)
		  (add-to-list 'company-include-path-user (expand-file-name (substring ede-include-path 1) (projectile-project-root)) t)
		  )
		company-include-path-user)
	(list "." "..")
	))

(require 'cc-mode)
(require 'function-args)
(fa-config-default)
(dolist (mode-map '(c-mode-map c++-mode-map))
  (define-key (eval mode-map) (kbd "<tab>") 'helm-yas-complete))

(dolist (mode-hook '(c-mode-hook c++-mode-hook))
  (add-hook mode-hook #'(lambda ()
						  (company-mode 1)
						  (local-set-key  (kbd "C-c o") 'cc-switch-source-header-file))))

;; company-c-headers
(add-to-list 'company-backends 'company-c-headers)
(setq company-c-headers-path-user 'cc-get-headers-path-user)
(setq company-c-headers-path-system 'cc-get-headers-path-system)

(provide 'init-c)
