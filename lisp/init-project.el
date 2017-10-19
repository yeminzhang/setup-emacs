(setq next-error-highlight t)

;; projectile
(use-package projectile
  :ensure t
  :defer t
  :config
  (setq projectile-completion-system 'helm
        projectile-mode-line '(" Proj" (:eval (spinner-print compile--spinner)))
        projectile-find-dir-includes-top-level t
        projectile-tags-command nil
        projectile-idle-timer-hook (list 'project-update-tags 'project-updatedb))
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
    (setq executable-file (ido-read-file-name "Executable: "))
    (setq executable-args (read-from-minibuffer "Parameters: "))
    (setq executable-envs (read-from-minibuffer "Envs: "))
    (project-save-attribute2 :executable-file executable-file)
    (project-save-attribute2 :executable-args executable-args)
    (project-save-attribute2 :executable-envs executable-envs))

  (defun project-load-attributes()
    (hack-dir-local-variables)
    (dolist (pair dir-local-variables-alist)
      (set (make-local-variable (car pair)) (cdr pair)))
    )

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

  (defun project-save-attribute2(&rest attributes)
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
          (project-save-attribute2 :run-cmd (gethash (projectile-project-root) projectile-run-cmd-map))))
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
        (project-save-attribute2 :debug-program debug-program))
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
          (project-save-attribute2 :compilation-cmd (gethash (projectile-project-root) projectile-compilation-cmd-map))))
    (project-update-tags)
    (project-updatedb)
    )

  (defun project-configure--tags-command ()
    (let
        ((project-tags-root (read-directory-name "GTAGS root dir:" (projectile-project-root))))
      (project-save-attribute 'projectile-tags-command (concat "cd " project-tags-root ";gtags"))))

  ;; Update GTAGS if it belongs to a project
  (defun project-update-tags ()
    (interactive)
    (let (
          ;; hardcoded now, to be fixed in future
          (projectile-tags-command (concat "cd " (projectile-project-root) ";gtags")))
      ;; (projectile-regenerate-tags) is a blocking function, so we use our own function
      (call-process-shell-command projectile-tags-command nil 0)))

  (defun project-updatedb ()
    (interactive)
    (when (projectile-project-p)
      (project-save-attribute2 :files
                               (split-string
                                (shell-command-to-string (format "find %s -type f -name \"*\"" (projectile-project-root)))
                                "\n" t))
      (project-save-attribute2 :dirs
                               (split-string
                                (shell-command-to-string (format "find %s -type d -name \"*\"" (projectile-project-root)))
                                "\n" t))
      ))

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
  :init
  (setq projectile-enable-idle-timer t
        projectile-idle-timer-seconds 1800)
  )

(set-display-buffer-other-window (rx bos "*Shell Command Output*" eos))

(projectile-global-mode 1)

;; ede for semantic
(global-ede-mode)

(provide 'init-project)
