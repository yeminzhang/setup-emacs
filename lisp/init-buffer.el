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
           if (and (stringp i) (if (tramp-tramp-file-p i) (s-ends-with-p "/" i) (file-directory-p i)))
           collect (if (s-ends-with-p "/" i) i (format "%s/" i))))

(defun keep-only-files (files)
  (cl-loop for i in files
           if (and (stringp i) (not (if (tramp-tramp-file-p i) (s-ends-with-p "/" i) (file-directory-p i))))
           collect i))

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
