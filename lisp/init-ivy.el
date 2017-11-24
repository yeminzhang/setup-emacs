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
            (if *is-mac*
                (s-replace ")" "" (s-replace "(" "" (counsel-unquote-regex-parens regex)))
              (counsel-unquote-regex-parens regex))))

  (defun counsel-locate-function (regex db-dir)
    (when (>= (length regex) 3)
      (if (and db-dir (file-exists-p (expand-file-name ".mlocate.db" db-dir)))
          (split-string
           (shell-command-to-string (counsel-locate-cmd regex (expand-file-name ".mlocate.db" db-dir)))
           "\n" t)
        nil)))

  (defun counsel-find-other-file-function (input)
    "find file for INPUT."
    (let (
          (regex (setq ivy--old-re (ivy--regex input))))
      (or
       (delete-dups
        (funcall counsel-file-filter
                 (append
                  (counsel-file-in-current-dir-function regex)
                  (counsel-recentf-function regex)
                  (counsel-locate-function regex "~") ;;global locate file
                  )))
       (list ""))))

  (defun counsel-find-project-file-function ()
    "find file for INPUT."
    (or
     (delete-dups
      (append
       (projectile-dir-files default-directory)
       (projectile-recentf-files)
       (projectile-current-project-files)))
     (list "")))

  (defun counsel-find-other-file ()
    (interactive)
    (let ((counsel-file-filter 'keep-only-files)
          (ivy--index 0))
      (ivy-read "find file: " 'counsel-find-other-file-function
                :dynamic-collection t
                :action 'find-file
                :caller 'counsel-find-other-file)))

  (defun counsel-find-project-file ()
    (interactive)
    (let ((ivy--index 0))
      (ivy-read "find file: " (counsel-find-project-file-function)
                :action (lambda (candidate)
                          (interactive)
                          (find-file (expand-file-name candidate (projectile-project-root))))
                :caller 'counsel-find-project-file)))

  (defun counsel-my-find-file (ARG)
    (interactive "P")
    (if ARG
        (counsel-find-other-file)
      (counsel-find-project-file)))

  (defun counsel-search-project-dir ()
    (interactive)
    (let ((ivy--index 0)
          (ivy-height 30))
      (ivy-read "search in dir: " (append '("./") (projectile-current-project-dirs))
                :action (lambda (candidate)
                          (interactive)
                          (counsel-ag "" (expand-file-name candidate (projectile-project-root))))
                :caller 'counsel-search-project-dir)))

  (defun counsel-search-other-dir ()
    (interactive)
    (let ((counsel-file-filter 'keep-only-dirs)
          (ivy--index 0)
          (ivy-height 30))
      (ivy-read "search in dir: " 'counsel-find-other-file-function
                :dynamic-collection t
                :action (lambda (candidate) (interactive) (counsel-ag "" candidate))
                :caller 'counsel-search-other-dir)))

  (defun counsel-my-search-dir (ARG)
    (interactive "P")
    (if ARG
        (counsel-search-other-dir)
      (counsel-search-project-dir)))

  (ivy-set-actions
   'counsel-search-other-dir
   '(("f"
      (lambda (candidate) (interactive) (find-file candidate))
      "open")))
  :bind (:map counsel-mode-map
              ("C-x C-f" . counsel-my-find-file)
              ("C-x C-d" . counsel-my-search-dir)
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
  :after projectile counsel ivy
  :config
  (define-key projectile-mode-map (kbd "C-c p g") 'counsel-projectile-ag)
  )

(use-package counsel-gtags
  :ensure t
  :diminish counsel-gtags-mode
  :bind (:map counsel-gtags-mode-map
              ("M-." . counsel-gtags-dwim)
              ("M-," . counsel-gtags-go-backward)
         )
  )

(ivy-mode t)
(counsel-mode t)

(provide 'init-ivy)
