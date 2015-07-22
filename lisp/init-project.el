(require-packages '(projectile helm-projectile ))
(setq compilation-auto-jump-to-first-error t)
(setq compilation-scroll-output 'first-error)
(setq next-error-highlight t)

(require 'gud)
(define-key gud-mode-map (kbd "C-q") 'project-debug-quit)

(defun project-save-attribute (symbol value)
  (let (
		(default-directory (projectile-project-root))
		(buffer-name (current-buffer))
		)
	(add-dir-local-variable nil symbol value)
	(save-buffer)
	(bury-buffer)
	(set-buffer buffer-name)
	))

(defun project-set-running-command ()
  (project-save-attribute 'project-executable-file (ido-read-file-name "Executable: "))
  (project-save-attribute 'project-executable-parameters (read-from-minibuffer "Parameters: "))
  (project-save-attribute 'project-executable-envs (read-from-minibuffer "Envs: "))
  )

(defun project-load-attributes()
  (hack-dir-local-variables)
  (dolist (pair dir-local-variables-alist)
	(set (make-local-variable (car pair)) (cdr pair)))
)

(defun project-run (ARG)
  (interactive "P")
  (project-load-attributes)
  (if (or ARG (not (boundp 'project-executable-file)))
	  (project-set-running-command)
	)
  (project-load-attributes)
  (shell-command (concat project-executable-file " " project-executable-parameters)))

(defun project-debug (ARG)
  (interactive "P")
  (project-load-attributes)
  (if (not (boundp 'project-debug-program))
	  (project-save-attribute 'project-debug-program (ido-completing-read "Debugger: " (list "gdb"))))
	(if (or ARG (not (boundp 'project-executable-file)))
		(project-set-running-command)
	  )
	(project-load-attributes)
	(gud-save-window-configuration)
	(if (string= project-debug-program "gdb") (cc-debug project-executable-file project-executable-parameters project-executable-envs))
	)

(defun project-debug-quit ()
  (interactive ())
  (if (eq gud-minor-mode 'gdbmi) (cc-debug-quit))
  (gud-restore-window-configuration)
  (bury-buffer "*gud-main*")
  )

(defun gud-active-process ()
  (if (get-buffer-process gud-comint-buffer) t nil)
  )

(defun gud-send-command (command)
  (if (gud-active-process)
	  (comint-send-string gud-comint-buffer (concat command "\n"))
	))

(defun gud-save-window-configuration ()
  (setq window-configuration-before-gdb (current-window-configuration))
  )

(defun gud-restore-window-configuration ()
  (set-window-configuration window-configuration-before-gdb)
  )

(defun project-compile (ARG)
  (interactive "P")
  (project-load-attributes)
  (projectile-compile-project ARG)
  (if ARG
	  (project-save-attribute 'projectile-project-compilation-cmd (gethash (projectile-project-root) projectile-compilation-cmd-map))))

;; Close the compilation window if there was no error at all.
(setq compilation-exit-message-function
      (lambda (status code msg)
	;; If M-x compile exists with a 0
	(when (and (eq status 'exit) (zerop code))
	  ;; then bury the *compilation* buffer, so that C-x b doesn't go there
  	  (bury-buffer "*compilation*")
  	  ;; and return to whatever were looking at before
  	  (replace-buffer-in-windows "*compilation*"))
	  (project-update-tags)
	;; Always return the anticipated result of compilation-exit-message-function
  	(cons msg code)))

(defun project-set-tags-root ()
  (interactive)
  (if (projectile-project-root)
	  (project-save-attribute 'project-tags-root (read-directory-name "dir:" (projectile-project-root)))))

;; Update TAGS if it belongs to a project
(defun project-update-tags ()
  (interactive)
  (project-load-attributes)
  (if (projectile-project-root)
	  (let (
			(tags-root (if (boundp 'project-tags-root) project-tags-root (projectile-project-root))))
	  (call-process-shell-command (concat "cd " tags-root ";" projectile-tags-command) nil 0))))

(defun project-configure-cpp-include-path ()
  (interactive)
  (project-load-attributes)
  (if (projectile-project-root)
	  (let (
			(tags-root (if (boundp 'project-tags-root) project-tags-root (projectile-project-root))))
		(call-process-shell-command (concat "cd " tags-root ";" projectile-tags-command) nil 0))))

;; projectile
(require 'projectile)
(projectile-global-mode 1)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq compilation-read-command nil)
(global-set-key (kbd "<f5>") 'project-compile)
(global-set-key (kbd "<f6>") 'project-run)
(global-set-key (kbd "<f7>") 'project-debug)
(setq projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))
(define-key projectile-mode-map (kbd "C-c p g") 'helm-projectile-grep)
(setq projectile-find-dir-includes-top-level t)
(setq projectile-tags-command "gtags")

;; ede for semantic
(require 'ede)
(global-ede-mode)

(provide 'init-project)
