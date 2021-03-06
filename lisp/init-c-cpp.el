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

(use-package company-c-headers
  :ensure t
  :config
  (add-to-list 'company-backends 'company-c-headers))

(provide 'init-c-cpp)
