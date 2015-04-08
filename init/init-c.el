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

;; For C/C++ buffer local jump
(defun cc-jump-local ()
(interactive)
(let ((helm-candidate-number-limit nil)) (moo-jump-local))
)

(require 'cc-mode)
(require 'function-args)
(fa-config-default)
(dolist (mode-map '(c-mode-map c++-mode-map))
  (define-key (eval mode-map) (kbd (concat "C-" right-little-finger-key)) 'cc-jump-local)
  (define-key (eval mode-map) (kbd "<tab>") 'helm-yas-complete))

(dolist (mode-hook '(c-mode-hook c++-mode-hook))
  (add-hook 'mode-hook #'(lambda () (company-mode 1))))

(add-to-list 'company-backends 'company-c-headers)
(provide 'init-c)
