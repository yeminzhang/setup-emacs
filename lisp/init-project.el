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
	(delete-dir-local-variable nil symbol)
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
  (projectile-save-project-buffers)
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
  (projectile-compile-project (if (bound-and-true-p projectile-project-compilation-cmd) ARG t))
  (if (or ARG (not projectile-project-compilation-cmd))
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

(defun project-configure--tags-command ()
  (let
	  ((project-tags-root (read-directory-name "GTAGS root dir:" (projectile-project-root))))
	(project-save-attribute 'projectile-tags-command (concat "cd " project-tags-root ";gtags"))))

;; Update GTAGS if it belongs to a project
(defun project-update-tags ()
  (interactive)
  (when (projectile-project-p)
	(project-load-attributes)
	(if (bound-and-true-p projectile-tags-command)
		(call-process-shell-command projectile-tags-command nil 0))))

(defun project-configure (ARG)
  (interactive "P")
  (when (projectile-project-p)
	(project-load-attributes)
	(let (
		  (project-type (if (or (not projectile-project-type) ARG) (ido-completing-read "project type: " '("C/C++" "other")) projectile-project-type)))
	  (if (y-or-n-p "Do you want to set the tags command? ") (project-configure--tags-command))
	  (cond
	   ((or (string= "C/C++" project-type) (eq 'c/c++ project-type)) (project-configure--cpp-project))
	   ;; For future expansion here
	   ))))

(defun project-configure--cpp-project ()
  (let* (
		 (ede-cpp-root-project-local (ede-cpp-root-load (projectile-project-root)))
		 (ede-include-path-user (split-string (read-from-minibuffer "ede include path user:" (if ede-cpp-root-project-local (s-join " " (oref ede-cpp-root-project-local :include-path)) nil))))
		 (ede-include-path-system (split-string (read-from-minibuffer "ede include path system:" (if ede-cpp-root-project-local (s-join " " (oref ede-cpp-root-project-local :system-include-path)) nil)))))
	(project-save-attribute 'projectile-project-type 'c/c++)
	(project-save-attribute 'eval (list ede-cpp-root-project (projectile-project-root)
										:file (expand-file-name ".dir-locals.el" (projectile-project-root))
										:include-path (cons 'list ede-include-path-user)
										:system-include-path (cons 'list ede-include-path-system)))
	(ede-cpp-root-project (projectile-project-root)
						  :file (expand-file-name ".dir-locals.el" (projectile-project-root))
						  :include-path ede-include-path-user
						  :system-include-path ede-include-path-system)
	(dolist (buffer (projectile-project-buffer-names))
	  (with-current-buffer buffer (project-load-attributes)))))

;; projectile
(require 'projectile)
(projectile-global-mode 1)
(setq projectile-completion-system 'helm)
(setq compilation-read-command nil)
(global-set-key (kbd "<f5>") 'project-compile)
(global-set-key (kbd "<f6>") 'project-run)
(global-set-key (kbd "<f7>") 'project-debug)
(setq projectile-mode-line '(:eval (if (projectile-project-p) (format " Proj[%s]" (projectile-project-name)) "")))
(helm-projectile-on)
(define-key projectile-mode-map (kbd "C-c p g") 'helm-projectile-grep)
(define-key projectile-mode-map (kbd "C-c p c") 'project-configure)
(define-key projectile-mode-map (kbd "C-c p R") 'project-update-tags)
(setq projectile-find-dir-includes-top-level t)
(setq projectile-tags-command nil)
(setq projectile-idle-timer-hook (list 'project-update-tags))

(custom-set-variables '(projectile-enable-idle-timer t))

;; ede for semantic
(require 'ede)
(global-ede-mode)

(setq compilation-ask-about-save nil)

(provide 'init-project)
