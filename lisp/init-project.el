(setq next-error-highlight t)

;; projectile
(use-package projectile
  :ensure t
  :defer t
  :config
  (setq projectile-completion-system 'ivy
        projectile-find-dir-includes-top-level t
        projectile-tags-command nil
        projectile-switch-project-action 'projectile-project-buffers-other-buffer
        projectile-enable-caching t
        projectile-mode-line-prefix " Proj"
        projectile-mode-line-function 'project-mode-line)

  (defun project-mode-line ()
    "Report project name and type in the modeline."
    (let ((project-name (projectile-project-name)))
      (format "%s[%s]"
              projectile-mode-line-prefix
              (or project-name "-"))))

  (defun project-relative-filename (filename)
    (message filename)
    (if (and (projectile-project-p) (projectile-file-cached-p filename (projectile-project-name)))
        (file-relative-name filename (projectile-project-root))
      filename))

  (defun project-switch-to-last-project ()
    (interactive)
    (projectile-switch-project-by-name (car (projectile-relevant-open-projects))))

  (defun project-set-running-command ()
    (setq executable-file (ido-read-file-name "Executable: "))
    (setq executable-args (read-from-minibuffer "Parameters: "))
    (setq executable-envs (read-from-minibuffer "Envs: "))
    (project-save-attribute :executable-file executable-file)
    (project-save-attribute :executable-args executable-args)
    (project-save-attribute :executable-envs executable-envs))

  (customize-save-default 'project-list '())

  (defun project-get-plist()
    (let ((result nil))
      (dolist (project project-list)
        (when (string= (projectile-project-root) (plist-get project :root))
          (setq result project)))
      result))

  (defun project-get-attribute(&rest attribute)
    (let (
          (project-plist (project-get-plist)))
      (if project-plist
          (plist-get project-plist (car attribute))
        nil)))

  (defun project-save-attribute(&rest attributes)
    (let (
          (project-plist (project-get-plist)))
      (if project-plist
          (delete project-plist project-list)
        (setq project-plist (plist-put project-plist :root (projectile-project-root))))
      (add-to-list 'project-list (plist-put project-plist (car attributes) (car (cdr attributes))))
      (customize-save-variable 'project-list project-list)))

  (defun project-run (ARG)
    (interactive "P")
    (let ((projectile-project-run-cmd (project-get-attribute :run-cmd)))
      (setq compilation-finish-functions nil)
      (projectile-run-project (if projectile-project-run-cmd ARG t))
      (if (or ARG (not projectile-project-run-cmd))
          (project-save-attribute :run-cmd (gethash (projectile-project-root) projectile-run-cmd-map))))
    )

  (defun project-debug (ARG)
    (interactive "P")
    (require 'gud)
    (let (
          (executable-file (project-get-attribute :executable-file))
          (executable-args (project-get-attribute :executable-args))
          (executable-envs (project-get-attribute :executable-envs))
          (debug-program (project-get-attribute :debug-program))
          (debug-prerun (project-get-attribute :debug-prerun)))
      (when (not debug-program)
        (setq debug-program (ido-completing-read "Debugger: " (list "gdb")))
        (project-save-attribute :debug-program debug-program))
      (if (or ARG (not executable-file))
          (project-set-running-command))
      (if (or ARG (not debug-prerun))
          (setq debug-prerun (read-from-minibuffer "Pre-run: ")))
      (gud-save-window-configuration)
      (if debug-prerun
          (shell-command debug-prerun))
      (if (string= debug-program "gdb") (cc-debug executable-file executable-args executable-envs))))

  (defun project-debug-quit ()
    (interactive ())
    (if (eq gud-minor-mode 'gdbmi) (cc-debug-quit))
    (gud-restore-window-configuration)
    (bury-buffer gud-comint-buffer)
    )

  (defun project-get-compilation-buffer-name (mode)
    (concat "*" (downcase mode) "-" (projectile-project-name) "*"))

  (defun project-compile (ARG)
    (interactive "P")
    (let ((projectile-project-compilation-cmd (project-get-attribute :compilation-cmd)))
      (setq compilation-finish-functions 'compile-autoclose)
      (projectile-compile-project (if projectile-project-compilation-cmd ARG t))
      (if (or ARG (not projectile-project-compilation-cmd))
          (project-save-attribute :compilation-cmd (gethash (projectile-project-root) projectile-compilation-cmd-map))))
    (project-update-tags)
    )

  (defun project-ag (ARG)
    (interactive "P")
    (let ((project-ag-option (if ARG (read-from-minibuffer "ag option: ") (project-get-attribute :ag-option))))
      (if ARG (project-save-attribute :ag-option project-ag-option))
      (counsel-projectile-ag project-ag-option)))

  ;; Update GTAGS if it belongs to a project
  (defun project-update-tags ()
    (interactive)
    (let (
          ;; hardcoded now, to be fixed in future
          (projectile-tags-command (concat "cd " (projectile-project-root) ";gtags")))
      ;; (projectile-regenerate-tags) is a blocking function, so we use our own function
      (call-process-shell-command projectile-tags-command nil 0)))

  ;; put project buffers in the beginning of a buffer list
  (defun project-prioritized-buffer-list ()
    (let* ((project-root (projectile-project-root))
           (project-buffer-names
            (mapcar #'buffer-name (cl-remove-if-not
                                   (lambda (buffer)
                                     (and (projectile-project-buffer-p buffer project-root) (not (get-buffer-window buffer))))
                                   (buffer-list))))
           (other-buffer-names
            (mapcar #'buffer-name (cl-remove-if
                                   (lambda (buffer)
                                     (or (projectile-project-buffer-p buffer project-root) (get-buffer-window buffer)))
                                   (buffer-list)))))
      (append project-buffer-names other-buffer-names)))

  (defun project-switch-to-buffer ()
    (interactive)
    (if (projectile-project-p)
        (let ((projectile-completion-system 'ido))
          (switch-to-buffer (projectile-completing-read "" (project-prioritized-buffer-list))))
      (ido-switch-buffer)))

  (global-set-key (kbd "C-x b") 'project-switch-to-buffer)

  :init
  (setq projectile-enable-idle-timer nil)
  :bind (:map projectile-mode-map
              ("C-c p R" . project-update-tags)
              ("C-c p c" . project-compile)
              ("C-c p r" . project-run)
              ("C-c p d" . project-debug)
              ("C-c p TAB" . project-switch-to-last-project)
              ("C-c p a" . projectile-find-other-file)
  ))

(set-display-buffer-other-window (rx bos "*Shell Command Output*" eos))

(projectile-mode 1)

(provide 'init-project)
