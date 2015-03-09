(defun project-compile (ARG)
  (interactive "P")
  (save-some-buffers 1)
  (projectile-compile-project ARG))

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

;; Update TAGS if it belongs to a project
(defun project-update-tags ()
  (interactive)
  (if (projectile-project-root)
	  (call-process-shell-command (concat "cd " (projectile-project-root) ";" projectile-tags-command) nil 0)))

;; Useless, To be removed in the future
(defun project-find-tag ()
  (interactive)
      (progn
	(if (string= "erlang-mode" major-mode) (erlang-find-tag-under-point))
	(if (string= "emacs-lisp-mode" major-mode) (elisp-find-function-under-point))
	))

(defun project-run ()
  (interactive)
      (progn
;;	(if (string= "erlang-mode" major-mode) (erlang-find-tag-under-point))
	(if (string= "emacs-lisp-mode" major-mode) (elisp-save-and-eval-buffer))
	))

(require 'helm-gtags)

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 helm-gtags-mode-name " HG"
 )

;; Enable helm-gtags-mode in Dired so you can jump to any tag
;; when navigate project tree with Dired
(add-hook 'dired-mode-hook 'helm-gtags-mode)

;; Enable helm-gtags-mode in Eshell for the same reason as above
(add-hook 'eshell-mode-hook 'helm-gtags-mode)

;; Enable helm-gtags-mode in languages that GNU Global supports
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'java-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; key bindings
(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)


;; projectile
(require 'projectile)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq compilation-read-command nil)
(global-set-key (kbd "<f5>") 'project-compile)
(setq projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))
(define-key projectile-mode-map (kbd "C-c p g") 'helm-projectile-grep)
(setq projectile-find-dir-includes-top-level t)
(setq projectile-tags-command "gtags")

(provide 'init-project)
