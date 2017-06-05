;; uniquify
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-strip-common-suffix t))

(global-auto-revert-mode 1)

(use-package autorevert
  :config
  (setq auto-revert-interval 1
        auto-revert-verbose nil
        auto-revert-remote-files t))

;; now only elisp buffer file is supported
(defun buffer-file-run ()
  (interactive)
  (when (eq major-mode 'emacs-lisp-mode) (elisp-save-and-eval-buffer)))

;; force all buffers to be displayed in the same window
;;(setq same-window-buffer-names '("*eshell*"))
(setq Man-notify-method 'pushy)

(setq enable-local-variables :all)

;; ido
(use-package ido
  :defer t
  :config
  (setq ido-enable-flex-matching t
        ido-enable-dot-prefix t
        ido-enable-regexp nil
        ido-ignore-extensions nil
        ido-max-window-height 1
        max-mini-window-height 1)
  (add-to-list 'ido-ignore-buffers "\*helm")
  (add-to-list 'ido-ignore-buffers "\*magit")
  (add-to-list 'ido-ignore-buffers "TAGS")
  (add-to-list 'ido-ignore-buffers "\*tramp")
  (defun ido-common-bind-key ()
    (define-key ido-common-completion-map (kbd ";") 'ido-exit-minibuffer)
    (define-key ido-common-completion-map (kbd "SPC") 'ido-next-match)
    (define-key ido-common-completion-map (kbd ",") 'ido-prev-match))
  (add-hook 'ido-minibuffer-setup-hook 'ido-common-bind-key))
(ido-mode 'both)

(defun keep-only-dirs (files)
  (cl-loop for i in files
           if (and (stringp i) (file-directory-p i))
           collect i))

(defun keep-only-files (files)
  (cl-loop for i in files
           if (and (stringp i) (not (file-directory-p i)))
           collect i))

;; By default regexp is not used. Add -r in a helm session to enable it
(defun make-locate-command (ARG)
  (let (
        (locate-db-file
         (if (or ARG
                 (not (projectile-project-p))
                 (not (file-exists-p (expand-file-name ".mlocate.db" (projectile-project-root)))))
             locate-db-file
           (expand-file-name ".mlocate.db" (projectile-project-root)))))
    (concat "locate %s -d " locate-db-file " -e -A %s")))

(setq locate-db-file "~/.mlocate.db")
(customize-save-default 'updatedb-option "-l 0")

(defun updatedb ()
  (interactive)
  (call-process-shell-command (concat "updatedb " updatedb-option " -o " locate-db-file) nil 0))

;; updatedb every 30 minutes
(unless (boundp 'updatedb-timer)
  (run-with-timer 1800 1800 'updatedb)
  (setq updatedb-timer t))

(updatedb)

;; recentf
(use-package recentf-ext
  :ensure t
  :config
  (setq recentf-max-menu-items 100
        recentf-max-saved-items nil
        recentf-auto-cleanup 'never ;; in order to keep tramp files
        ))

;; auto-save file
;; Save all tempfiles in ~/.emacs-tmp/
(setq temporary-file-directory  "~/.emacs-tmp/")
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq auto-save-interval 100)
(setq auto-save-timeout 10)

(provide 'init-buffer)
