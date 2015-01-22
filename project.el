(defun project-get-id (filename)
  (if (eq nil filename) nil
    (progn
      (setq expanded-filename (expand-file-name filename))
      (setq local-project-list project-list)
      (setq result nil)
      (while local-project-list
	(setq project (pop local-project-list))
	(setq project-root-dir (expand-file-name (plist-get project :root-dir)))
	(if (string= (substring expanded-filename 0 (length project-root-dir)) project-root-dir)
	    (setq result (plist-get project :id))
	  ))
      result
      )))

(defun project-get-attribute (project-id attribute-name)
  (progn
    (setq local-project-list project-list)
    (setq result nil)
    (while local-project-list
      (setq project (pop local-project-list))
      (setq id (plist-get project :id))
      (if (string= id project-id)
	  (setq result (plist-get project attribute-name))
	))
    result
    ))

(defun project-compile ()
  (interactive)
  (progn
    (setq project-id (project-get-id buffer-file-name))
    (if (eq nil project-id) (recompile)
      (progn
	(setq configured-command (project-get-attribute project-id :compile-command))
	(setq root-dir (project-get-attribute project-id :root-dir))
	(setq command (if (eq nil configured-command) "make" configured-command))
	(save-some-buffers 1)
	(setq current-project-id project-id)
	(compile (concat "cd " root-dir "; " command))
	))
    ))

;; Close the compilation window if there was no error at all.
(setq compilation-exit-message-function
      (lambda (status code msg)
	;; If M-x compile exists with a 0
	(when (and (eq status 'exit) (zerop code))
	  ;; then bury the *compilation* buffer, so that C-x b doesn't go there
  	  (bury-buffer "*compilation*")
  	  ;; and return to whatever were looking at before
  	  (replace-buffer-in-windows "*compilation*")
	  ;; Update TAGS if it belongs to a project
	  (project-update-tags current-project-id))
	;; Always return the anticipated result of compilation-exit-message-function
  	(cons msg code)))


;; Do this when just create the project on disk. Otherwise project-find-tag won't work
(defun project-update-tags (project-id)
  (interactive (list (project-get-id buffer-file-name)))
    (unless (eq nil project-id)
     (shell-command (concat "cd " (project-get-attribute project-id :root-dir) "; " (project-get-attribute project-id :tag-command)))))

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

;; grep under :grep-root of a project
(defun project-grep ()
  (interactive)
  (require 'helm-mode)
  (let* ((project-id (project-get-id buffer-file-name))
(only (if project-id (or (split-string (project-get-attribute project-id :grep-root) " ") (list (project-get-attribute project-id :root-dir))) nil)))
    (if project-id (helm-do-grep-1 only t) (message "not inside a project!"))))

(global-set-key (kbd "<f5>") 'project-compile)
(global-set-key (kbd "<f6>") 'project-run)
(global-set-key (kbd "M-.") 'project-find-tag)
(global-set-key (kbd "M-,") 'pop-tag-mark)

(add-to-list 'load-path (concat emacs-configuration-root-dir "helm-gtags"))
(require 'helm-gtags)

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
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

(provide 'project)
