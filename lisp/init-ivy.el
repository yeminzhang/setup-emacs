(use-package swiper
  :ensure t
  :after ivy-mode
  :config
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "C-r") 'swiper)
  )

(use-package counsel
  :ensure t)

(use-package ivy
  :ensure t
  :config
  (setq ivy-height 20)
  :bind (:map ivy-mode-map
         ("C-x C-f" . counsel-my-find-file)
         :map ivy-minibuffer-map
         (";" . ivy-done)
         )
  )

(use-package counsel-projectile
  :ensure t
  :after projectile ivy-mode
  :config
  (define-key projectile-mode-map (kbd "C-c p g") 'counsel-projectile-rg)
  )

(defun counsel-find-file-in-current-dir-function (input)
  (let* (
         (regex (ivy--regex input)))
    (directory-files default-directory t regex)))

(defun counsel-find-recentf (input)
  (require 'recentf)
  (recentf-mode)
  (let* (
         (recentfs (mapcar #'substring-no-properties recentf-list))
         (matched-list '())
         (regex (ivy--regex input)))
    (dolist (recentf recentfs)
      (when (string-match regex recentf) (push recentf matched-list )))
    matched-list))

(defun counsel-locate-project-cmd (input)
  "Return a shell command based on INPUT."
  (format "locate -i -e -A -l 50 --regex -d %s '%s'"
          (expand-file-name ".mlocate.db" (projectile-project-root))
          (counsel-unquote-regex-parens
           (ivy--regex input))))

(defun counsel-find-project-file (input)
  (if (< (length input) 3)
      (counsel-more-chars 3)
    (let (
          (project-root (locate-dominating-file default-directory ".mlocate.db")))
      (when project-root
         (split-string
          (shell-command-to-string (counsel-locate-project-cmd input))
          "\n" t)))))

(defun counsel-find-file-function (input)
  "find file for INPUT."
  (or
   (delete-dups
    (append
     (counsel-find-file-in-current-dir-function input)
     (counsel-find-recentf input)
     (counsel-find-project-file input)
     ))
   (list "")))

(defun counsel-my-find-file ()
  "Grep for a string in the current file."
  (interactive)
    (ivy-read "find file: " 'counsel-find-file-function
              :dynamic-collection t
              :action 'find-file
              :caller 'counsel-my-find-file))

(ivy-mode t)

(provide 'init-ivy)
