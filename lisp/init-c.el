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

(require 'cc-mode)
(require 'function-args)
(fa-config-default)
(dolist (mode-map '(c-mode-map c++-mode-map))
  (define-key (eval mode-map) (kbd "<tab>") 'helm-yas-complete))

(dolist (mode-hook '(c-mode-hook c++-mode-hook))
  (add-hook mode-hook #'(lambda ()
						  (company-mode 1)
						  (local-set-key  (kbd "C-c o") 'cc-switch-source-header-file))))

(add-to-list 'company-backends 'company-c-headers)
(provide 'init-c)
