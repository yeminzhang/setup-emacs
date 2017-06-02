;; Tabsn
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

;; whitespace
(setq-default show-trailing-whitespace nil)
(use-package whitespace
  :defer t
  :config
  (setq whitespace-style '(tab-mark)))  ;;turns on white space mode only for tabs
(add-to-list 'write-file-hooks 'delete-trailing-whitespace)

(setq inhibit-field-text-motion t)
(use-package misc
  :bind (("M-f" . forward-to-word)))

;; xclip
(setq x-select-enable-clipboard t)

;; smooth scrolling
(setq scroll-conservatively 101)

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

(use-package smex
  :ensure t
  :bind ("M-x" . smex)
  :config
  ;; auto update smex cache after load a file
  (defun smex-update-after-load (unused)
    (when (boundp 'smex-cache)
      (smex-update)))
  (add-hook 'after-load-functions 'smex-update-after-load))

(use-package ediff-wind
  :defer t
  :config
  (setq ediff-split-window-function 'split-window-horizontally))

;; show number of matches in current search
(use-package anzu
  :ensure t
  :defer t
  :diminish anzu-mode)
(global-anzu-mode t)

(defun maybe-split-window (&optional switch-window)
  (when (one-window-p t)
    (split-window-horizontally))
  (when switch-window) (other-window 1))

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

(use-package desktop
  :defer t
  :config
  (add-to-list 'desktop-globals-to-save 'kill-ring))

(defun smart-kill-ring-save (&optional arg)
  "When called interactively with no active region, copy the whole line."
  (interactive "^p")
  (if mark-active
      (if (= (region-beginning) (region-end))
          ;; if mark is active but no text is selected, then copy the whole word
          (progn
            (deactivate-mark)
            (call-interactively 'forward-to-word)
            (call-interactively 'backward-word)
            (call-interactively 'mark-sexp)
            (smart-kill-ring-save))
        ;; copy selected region
        (kill-ring-save (region-beginning) (region-end)))
    ;; copy the whole line
    (kill-ring-save (line-beginning-position) (line-beginning-position (+ arg 1)))))

(defun smart-kill-region (&optional arg)
  "When called interactively with no active region, kill the whole line."
  (interactive "^p")
  (if mark-active (kill-region (region-beginning) (region-end))
    (kill-region (line-beginning-position) (line-beginning-position (+ arg 1)))))

(defadvice insert-for-yank (before newline-if-is-linestr (str) activate)
  (when (char-equal (aref str (- (length str) 1)) ?\n)
    (end-of-line)
    (if (= (point) (point-max))
        ;; if it reaches end of buffer, then create a newline
        (newline)
      ;; otherwise we just yank line before beginning of next line
      (call-interactively 'forward-char))))

;; auto indent when paste something
(defadvice insert-for-yank (after indent-region (str) activate)
   (and (not current-prefix-arg)
        (derived-mode-p 'prog-mode)
        (let ((mark-even-if-inactive transient-mark-mode))
          (indent-region (region-beginning) (region-end) nil))))

;; undo-tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode)
;; volatile-highlights
(volatile-highlights-mode t)

(use-package ace-jump-mode
  :ensure t
  :bind (("C-j" . ace-jump-mode)
         ))

(use-package ace-isearch
  :ensure t
  :disabled t
  :diminish ace-isearch-mode
  :defer t
  :config
  (setq ace-isearch-use-jump 'never
        ace-isearch-func-delay 1
        ace-isearch-jump-delay 0.8)
  )

;; auto-save file
;; Save all tempfiles in ~/.emacs-tmp/
(setq temporary-file-directory  "~/.emacs-tmp/")
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq auto-save-interval 100)
(setq auto-save-timeout 10)

;; recentf
(use-package recentf-ext
  :ensure t
  :config
  (setq recentf-max-menu-items 100
        recentf-max-saved-items nil
        recentf-auto-cleanup 'never ;; in order to keep tramp files
        ))

;; bookmark+
(defun bookmark-load-if-not ()
  (use-package bookmark+
    :ensure t)
  (if (and (not bookmarks-already-loaded) (file-readable-p bookmark-default-file))
      (bookmark-load bookmark-default-file)))

(use-package iedit
  :ensure t)

;; Auto save bookmark to file every 8 modifications
(setq bookmark-save-flag 8)

(set-language-environment "UTF-8")

(updatedb)

(provide 'init-edit)
