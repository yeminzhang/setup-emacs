;; init-c-cpp is for c-mode, c++-mode

;;(require 'flymake-cursor)
;;(add-hook 'find-file-hook 'flymake-find-file-hook)

(defun flymake-simple-make-init ()
  (list "make"
        (list "-s" "-C" "./")))

(setq gdb-speedbar-auto-raise t)

(defun cc-debug (executable parameters envs)
  (setq command-line (concat "gdb -i=mi -cd=" (projectile-project-root) " " executable)
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
  (with-current-buffer gud-comint-buffer
    (concat "/tmp/" (s-replace "/" "!" project-executable-file))))

(defun cc-debug-quit ()
  (interactive ())
  (gud-send-command (concat "save breakpoints " (cc-breakpoints-filename)))
  (gud-send-command "quit")
  )

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

(use-package company-c-headers
  :ensure t
  :config
  (setq company-c-headers-path-user 'cc-get-headers-path-user
        company-c-headers-path-system 'cc-get-headers-path-system)
  (add-to-list 'company-backends 'company-c-headers))

(provide 'init-c-cpp)
