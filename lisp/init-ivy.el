(use-package swiper
  :ensure t
  :after ivy-mode
  :config
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "C-r") 'swiper)
  )

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :config
  (defun counsel-match-default (regex source-list)
    (let* (
           (matched-list '()))
      (cl-loop for i in source-list
               if (and (stringp i) (string-match regex i))
               collect i)))

  (defun counsel-file-in-current-dir-function (regex)
    (counsel-match-default regex (directory-files default-directory t)))

  (defun counsel-recentf-function (regex)
    (require 'recentf)
    (recentf-mode t)
    (counsel-match-default regex (mapcar #'substring-no-properties recentf-list)))

  (defun counsel-locate-cmd (regex db-path)
    "Return a shell command based on INPUT."
    (format "locate -i -e -A --regex -d %s '%s'"
            db-path
            (counsel-unquote-regex-parens regex)))

  (defun counsel-locate-function (regex db-dir)
    (when (>= (length regex) 3)
      (if (and db-dir (file-exists-p (expand-file-name ".mlocate.db" db-dir)))
          (split-string
           (shell-command-to-string (counsel-locate-cmd regex (expand-file-name ".mlocate.db" db-dir)))
           "\n" t)
        nil)))

  (defun counsel-find-file-function (input)
    "find file for INPUT."
    (let (
          (regex (setq ivy--old-re (ivy--regex input))))
      (or
       (delete-dups
        (funcall counsel-file-filter
                 (append
                  (counsel-file-in-current-dir-function regex)
                  (counsel-recentf-function regex)
                  (counsel-locate-function regex (locate-dominating-file default-directory ".mlocate.db")) ;;project files
                  (counsel-locate-function regex "~") ;;global locate file
                  )))
       (list ""))))

  (defun counsel-my-find-file ()
    (interactive)
    (let ((counsel-file-filter 'keep-only-files)
          (ivy--index 0))
      (ivy-read "find file: " 'counsel-find-file-function
                :dynamic-collection t
                :action 'find-file
                :caller 'counsel-my-find-file)))

  (defun counsel-search-dir ()
    (interactive)
    (let ((counsel-file-filter 'keep-only-dirs)
          (ivy--index 0)
          (ivy-height 30))
      (ivy-read "search in dir: " 'counsel-find-file-function
                :dynamic-collection t
                :action (lambda (candidate) (interactive) (counsel-ag "" candidate))
                :caller 'counsel-search-dir)))
  (ivy-set-actions
   'counsel-search-dir
   '(("f"
      (lambda (candidate) (interactive) (find-file candidate))
      "open")))
  :bind (:map counsel-mode-map
              ("C-x C-f" . counsel-my-find-file)
              ("C-x C-d" . counsel-search-dir)
              ))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (setq ivy-height 20)
  :bind (:map ivy-minibuffer-map
         (";" . ivy-done)
         )
  )

(use-package counsel-projectile
  :ensure t
  :after projectile counsel-mode ivy-mode
  :config
  (define-key projectile-mode-map (kbd "C-c p g") 'counsel-projectile-ag)
  )

(use-package counsel-gtags
  :ensure t
  :diminish counsel-gtags-mode
  :bind (:map counsel-gtags-mode-map
              ("M-." . counsel-gtags-dwim)
              ("M-," . counsel-gtags-pop)
         )
  )

(ivy-mode t)
(counsel-mode t)

(provide 'init-ivy)
