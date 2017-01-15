(require-packages '(smex undo-tree volatile-highlights iedit bookmark+ evil recentf-ext anzu))

;; Tabsn
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

;; whitespace
(setq-default show-trailing-whitespace nil)
(after-load 'whitespace
  (setq whitespace-style '(tab-mark)))  ;;turns on white space mode only for tabs
(add-to-list 'write-file-hooks 'delete-trailing-whitespace)

(setq x-select-enable-clipboard t)

;; smooth scrolling
(setq scroll-conservatively 101)

;; ido
(after-load 'ido
  (setq ido-enable-flex-matching t)
  (setq ido-enable-dot-prefix t)
  (setq ido-enable-regexp nil)
  (setq ido-ignore-extensions nil)
  (add-to-list 'ido-ignore-buffers "\*helm")
  (add-to-list 'ido-ignore-buffers "\*magit")
  (add-to-list 'ido-ignore-buffers "TAGS")
  (add-to-list 'ido-ignore-buffers "\*tramp"))
(ido-mode 'both)

;; auto update smex cache after load a file
(defun smex-update-after-load (unused)
  (when (boundp 'smex-cache)
    (smex-update)))

(add-hook 'after-load-functions 'smex-update-after-load)

(after-load 'ediff-wind
  (setq ediff-split-window-function 'split-window-horizontally))

;; show number of matches in current search
(global-anzu-mode t)
(after-load 'anzu
  (diminish 'anzu-mode))

(defun maybe-split-window (&optional switch-window)
  (when (one-window-p t)
    (split-window-horizontally))
  (when switch-window) (other-window 1))

(defun helm-keep-only-dirs (files)
  (cl-loop for i in files
           if (and (stringp i) (file-directory-p i))
           collect i))

(defun helm-keep-only-files (files)
  (cl-loop for i in files
           if (and (stringp i) (not (file-directory-p i)))
           collect i))


(setq my-helm-source-recentf (copy-tree helm-source-recentf))
(setf (nth 0 my-helm-source-recentf) '(name . "Recent files excluing directories"))
(setf (nth 3 (nth 2 my-helm-source-recentf)) '(delete-dups
                                               (mapcar (lambda (file)
                                                         (if (s-ends-with-p "/" file) "" file))
                                                       recentf-list)))

(setq my-helm-source-recentd (copy-tree helm-source-recentf))
(setf (nth 0 my-helm-source-recentd) '(name . "Recent directories"))
(setf (nth 3 (nth 2 my-helm-source-recentd)) '(delete-dups
                                               (mapcar (lambda (file)
                                                         (if (s-ends-with-p "/" file) file ""))
                                                       recentf-list)))

(setq helm-source-locate-dirs (copy-tree helm-source-locate))
(setf (nth 1 (nth 8 helm-source-locate-dirs)) 'helm-keep-only-dirs)

(setq helm-source-locate-files (copy-tree helm-source-locate))
(let ((candidate-transformer (nth 8 helm-source-locate-files)))
  (setcdr candidate-transformer (cons 'helm-keep-only-files (cdr candidate-transformer))))

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

(defun helm-find-file-internal (ARG prompt sources)
  (let ((helm-locate-command (make-locate-command ARG)))
    (helm
     :prompt prompt
     :candidate-number-limit 25                 ;; up to 25 of each
     :sources sources)))

(defun helm-find-file (ARG)
  (interactive "P")
  (helm-find-file-internal ARG
                           "Open file: "
                           '(  helm-source-files-in-current-dir
                               my-helm-source-recentf
                               helm-source-locate-files)))

(defun helm-find-dir (ARG)
  (interactive "P")
  (helm-find-file-internal ARG
                           "Open dir: "
                           '(my-helm-source-recentd
                             helm-source-locate-dirs)))

(setq locate-db-file "~/.mlocate.db")
(customize-save-default 'updatedb-option "-l 0")

(defun updatedb ()
  (interactive)
  (call-process-shell-command (concat "updatedb " updatedb-option " -o " locate-db-file) nil 0))

;; updatedb every 30 minutes
(unless (boundp 'updatedb-timer)
  (run-with-timer 1800 1800 'updatedb)
  (setq updatedb-timer t))

(defun insert-special-char (char_str)
  (interactive (list (ido-completing-read "Char to insert: " (list "ö" "ä" "å" "Ö" "Ä" "Å" "~"))))
  (insert char_str))

(after-load 'desktop
  (add-to-list 'desktop-globals-to-save 'kill-ring))

(require 'evil)

(defadvice evil-yank (after disable-evil activate)
  (evil-local-mode -1))

(defadvice evil-delete (after disable-evil activate)
  (evil-local-mode -1))

(defun smart-kill-ring-save ()
  "When called interactively with no active region, copy the whole line."
  (interactive)
  (if mark-active (kill-ring-save (region-beginning) (region-end))
    (call-interactively 'evil-yank-line)))

(defun smart-kill-region ()
  "When called interactively with no active region, kill the whole line."
  (interactive)
  (if mark-active (kill-region (region-beginning) (region-end))
    (call-interactively 'evil-delete-whole-line)))

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-mode-lighter "")

;; volatile-highlights
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; auto-save file
;; Save all tempfiles in ~/.emacs-tmp/
(setq temporary-file-directory  "~/.emacs-tmp/")
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq auto-save-interval 100)
(setq auto-save-timeout 10)

;; auto indent when paste something
(dolist (command '(yank yank-pop evil-paste-after evil-paste-before evil-paste-pop))
  (eval
   `(defadvice ,command (after indent-region activate)
      (and (not current-prefix-arg)
           (derived-mode-p 'prog-mode)
           (let ((mark-even-if-inactive transient-mark-mode))
             (indent-region (region-beginning) (region-end) nil))))))

;; xclip

;; recentf
(require 'recentf-ext)
(setq recentf-max-menu-items 100
      recentf-max-saved-items nil)

;; bookmark+
(defun bookmark-load-if-not ()
  (require 'bookmark+)
  (if (and (not bookmarks-already-loaded) (file-readable-p bookmark-default-file))
      (bookmark-load bookmark-default-file)))

;; Auto save bookmark to file every 8 modifications
(setq bookmark-save-flag 8)

(set-language-environment "UTF-8")

(updatedb)

(provide 'init-edit)
