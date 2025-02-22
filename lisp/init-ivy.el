(use-package swiper
  :ensure t
  :after ivy
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

  ;; redefine counsel-yank-pop-action, replace insert with insert-for-yank
  ;; in order to get same behavior as C-y
  (defun counsel-yank-pop-action (s)
    "Insert S into the buffer, overwriting the previous yank."
    (with-ivy-window
      (delete-region ivy-completion-beg
                     ivy-completion-end)
      (insert-for-yank (substring-no-properties s))
      (setq ivy-completion-end (point))))

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

  (defun counsel-find-other-file ()
    (interactive)
    (let ((counsel-file-filter 'keep-only-files)
          (ivy--index 0))
      (ivy-read "find file: " 'counsel-find-other-file-function
                :dynamic-collection t
                :action 'find-file
                :caller 'counsel-find-other-file)))

  (defun counsel-find-file-matcher (regexp candidates)
    (let ((dup-result (ivy--re-filter regexp candidates))
          (no-dup-result '()))
      (if (< (length dup-result) 10)
          (progn
            (dolist (cand dup-result)
              (add-to-list 'no-dup-result cand t))
            (delete-dups no-dup-result))
        dup-result)))

  (defun counsel-find-project-file ()
    (interactive)
    (ivy-read "find file: " (projectile-current-project-files)
              :action (lambda (candidate)
                        (interactive)
                        (find-file (expand-file-name candidate (projectile-project-root))))
              :preselect (when (buffer-file-name) (file-relative-name (buffer-file-name) (projectile-project-root)))
              :matcher 'counsel-find-file-matcher
              :caller 'counsel-find-project-file))

  (defun counsel-my-find-file (ARG)
    (interactive "P")
    (if (or ARG (not (projectile-project-p)))
        (counsel-find-other-file)
      (counsel-find-project-file)))

  (defun counsel-search-project-dir ()
    (interactive)
    (let ((ivy--index 0)
          (ivy-height 30))
      (ivy-read "search in dir: " (append (list (file-relative-name (expand-file-name "./") (projectile-project-root)))
                                          (projectile-current-project-dirs))
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
    (if (or ARG (not (projectile-project-p)))
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
  :bind (:map projectile-mode-map
  ("C-c p g" . project-ag)
  ("C-c p f" . counsel-projectile-find-file)
  ))

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
