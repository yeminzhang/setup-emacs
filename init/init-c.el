(add-to-list 'load-path (concat emacs-configuration-root-dir "xcscope"))
(require 'xcscope)
(setq next-error-highlight t)
;;(require 'flymake-cursor)
;;(add-hook 'find-file-hook 'flymake-find-file-hook)

(defun flymake-simple-make-init ()
(list "make"
(list "-s" "-C" "./")))

(require 'gud)
(define-key gud-mode-map (kbd "C-q") 'debug-mode-quit)
(setq gdb-speedbar-auto-raise t)
(setq compilation-auto-jump-to-first-error t)
(setq compilation-scroll-output 'first-error)

(defun gud-active-process ()
(if (get-buffer-process gud-comint-buffer) t nil)
)

(defun gud-send-command (command)
(if (gud-active-process) (comint-send-string gud-comint-buffer (concat command "\n")))
)

(defun gud-save-window-configuration ()
(setq window-configuration-before-gdb (current-window-configuration))
)

(defun gud-restore-window-configuration ()
(set-window-configuration window-configuration-before-gdb)
)

(defun gud-save-debug-environment (command &optional parameters env-list)
(setq gud-last-command command)
(setq gud-last-parameters parameters)
(setq gud-last-env-list env-list)
)

(defun run-program (executable-file &optional parameters envs)
(interactive (list (ido-read-file-name "Executable: ")
(read-from-minibuffer "Parameters: ")
(read-from-minibuffer "Envs: ")))
(debug-program executable-file nil parameters envs))

(defun debug-program (executable-file to-set-breakpoints &optional parameters envs)
(interactive (list (ido-read-file-name "Executable: ")
t
(read-from-minibuffer "Parameters: ")
(read-from-minibuffer "Envs: ")))
(gud-save-window-configuration)
(setq command-line (concat "gdb -i=mi " executable-file))
(gdb command-line)
(if to-set-breakpoints (gdb-many-windows nil))
(if parameters (gud-send-command (concat "set args " parameters)))
(if envs (progn
(setq env-list (split-string envs ","))
(dolist (env env-list)
(gud-send-command (concat "set environment " env)))))
(gdb-clear-inferior-io)
(if to-set-breakpoints (gud-send-command "break main"))
(gud-run 0)
(gud-save-debug-environment command-line parameters env-list)
)

(defun redebug-program (to-enable-breakpoints)
(interactive (list t))
(progn
(unless gdb-many-windows (gud-save-window-configuration))
(unless (gud-active-process) (gdb gud-last-command))
(with-current-buffer gud-comint-buffer
(if to-enable-breakpoints 
(progn  (gud-send-command "enable")
(unless gdb-many-windows (gdb-many-windows nil))
))
(if gud-last-parameters (gud-send-command (concat "set args " gud-last-parameters)))
(if gud-last-env-list 
(dolist (env gud-last-env-list)
(gud-send-command (concat "set environment " env))))
(gdb-clear-inferior-io)
(gud-run 0))))

(defun rerun-program ()
(interactive ())
(gud-send-command "disable")
(redebug-program nil)
)

(defun debug-mode-quit ()
(interactive ())
(if gdb-many-windows (gdb-many-windows nil))
(gud-restore-window-configuration)
)


(defun bury-compile-buffer-if-successful (buffer string)

"Bury a compilation buffer if succeeded without warnings "

(if (and

   (string-match "compilation" (buffer-name buffer))

   (string-match "finished" string)

   (not

    (with-current-buffer buffer

      (goto-char 1)

      (search-forward "warning" nil t))))

  (run-with-timer 1 nil

                  (lambda (buf)

                    (bury-buffer buf)

                    (switch-to-prev-buffer (get-buffer-window buf) 'bury))

                  buffer)))

;;(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)


  ;; Close the compilation window if there was no error at all.
  (setq compilation-exit-message-function
        (lambda (status code msg)
          ;; If M-x compile exists with a 0
          (when (and (eq status 'exit) (zerop code))
            ;; then bury the *compilation* buffer, so that C-x b doesn't go there
  	  (bury-buffer "*compilation*")
  	  ;; and return to whatever were looking at before
  	  (replace-buffer-in-windows "*compilation*"))
          ;; Always return the anticipated result of compilation-exit-message-function
  	(cons msg code)))


(global-set-key (kbd "<f5>") 'recompile)
(global-set-key (kbd "<f6>") 'rerun-program)
(global-set-key (kbd "<f7>") 'redebug-program)

(provide 'init-c)