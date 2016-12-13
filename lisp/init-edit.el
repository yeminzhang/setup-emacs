(require-packages '(smex undo-tree volatile-highlights iedit bookmark+ evil recentf-ext))

;; key bindings
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C--") 'undo)
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)
(global-set-key (kbd "<backspace>") 'backward-delete-char)
(global-set-key (kbd "M-j") 'delete-indentation)

;; Tabsn
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

;; whitespace
(setq-default show-trailing-whitespace nil)
(after-load 'whitespace
  (setq whitespace-style '(tab-mark)))  ;;turns on white space mode only for tabs
(add-to-list 'write-file-hooks 'delete-trailing-whitespace)

(global-set-key (kbd "C-x g") 'beginning-of-buffer)
(global-set-key (kbd "C-x G") 'end-of-buffer)

(setq x-select-enable-clipboard t)

;; ido
(after-load 'ido
  (setq ido-enable-flex-matching t)
  (setq ido-enable-dot-prefix t)
  (setq ido-enable-regexp nil)
  (setq ido-ignore-extensions nil)
  (add-to-list 'ido-ignore-buffers "\*helm")
  (add-to-list 'ido-ignore-buffers "\*magit")
  (add-to-list 'ido-ignore-buffers "TAGS")
  (add-to-list 'ido-ignore-buffers "\*tramp")
  (add-hook 'ido-minibuffer-setup-hook 'ido-common-bind-key))
(ido-mode 'both)

(defun ido-common-bind-key ()
  (define-key ido-common-completion-map (kbd right-little-finger-key) 'ido-exit-minibuffer)
  (define-key ido-common-completion-map (kbd "SPC") 'ido-next-match)
  (define-key ido-common-completion-map (kbd ",") 'ido-prev-match))

;; smex
(global-set-key (kbd "M-x") 'smex)

;; auto update smex cache after load a file
(defun smex-update-after-load (unused)
  (when (boundp 'smex-cache)
    (smex-update)))

(add-hook 'after-load-functions 'smex-update-after-load)

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

(setq my-helm-source-locate-dir (copy-tree helm-source-locate))
(setf (nth 1 (nth 8 my-helm-source-locate-dir)) 'helm-keep-only-dirs)

(setq my-helm-source-locate-files (copy-tree helm-source-locate))
(setf (nth 8 my-helm-source-locate-files) (append (list (car (nth 8 my-helm-source-locate-files))) '(helm-keep-only-files) (cdr (nth 8 my-helm-source-locate-files))))

;; By default regexp is not used. Add -r in a helm session to enable it
(defun make-locate-command (ARG)
  (let (
        (locate-db-file
         (if (or ARG (not (projectile-project-p)) (not (file-exists-p (expand-file-name ".mlocate.db" (projectile-project-root)))))
             locate-db-file
           (expand-file-name ".mlocate.db" (projectile-project-root)))))
    (concat "locate %s -d " locate-db-file " -e -A %s")))

(defun my-helm-find-file (ARG)
  (interactive "P")
  (let ((helm-locate-command (make-locate-command ARG)))
  (helm
   :prompt "Open file: "
   :candidate-number-limit 25                 ;; up to 25 of each
   :sources
   '(
     helm-source-files-in-current-dir ;; current dir
     my-helm-source-recentf               ;; recent files
;;     helm-source-projectile-files-list
     my-helm-source-locate-files))))            ;; use 'locate'

(defun my-helm-find-dir (ARG)
  (interactive "P")
  (let ((helm-locate-command (make-locate-command ARG)))
  (helm
   :prompt "Go to dir: "
   :candidate-number-limit 25                 ;; up to 25 of each
   :sources
   '(
;;     helm-source-projectile-directories-list
     my-helm-source-recentd
     my-helm-source-locate-dir))))

;; find a file
(global-set-key (kbd "C-x C-f") 'my-helm-find-file)
;; find a dir
(global-set-key (kbd "C-x C-d") 'my-helm-find-dir)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(setq locate-db-file "~/.mlocate.db")

(defun updatedb ()
  (interactive)
  (call-process-shell-command (concat "updatedb -o " locate-db-file " -l 0") nil 0))

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

(defun my-kill-ring-save ()
  "When called interactively with no active region, copy the whole line."
  (interactive)
  (if mark-active (kill-ring-save (region-beginning) (region-end))
    (call-interactively 'evil-yank-line)))

(defun my-kill-region ()
  "When called interactively with no active region, kill the whole line."
  (interactive)
  (if mark-active (kill-region (region-beginning) (region-end))
    (call-interactively 'evil-delete-whole-line)))

(global-set-key (kbd "C-w") 'my-kill-region)
(global-set-key (kbd "M-w") 'my-kill-ring-save)
(global-set-key (kbd "C-y") 'evil-paste-after)
(global-set-key (kbd "C-x Y") 'evil-paste-before)

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
           (member major-mode
                   '(emacs-lisp-mode
                     lisp-mode
                     clojure-mode
                     scheme-mode
                     haskell-mode
                     ruby-mode
                     rspec-mode
                     python-mode
                     c-mode
                     c++-mode
                     objc-mode
                     latex-mode
                     js-mode
                     plain-tex-mode
                     sh-mode
                     conf-unix-mode
                     erlang-mode))
           (let ((mark-even-if-inactive transient-mark-mode))
             (indent-region (region-beginning) (region-end) nil))))))


;; xclip

;; iedit
(global-set-key (kbd "C-,") 'iedit-mode)
(setq iedit-toggle-key-default (kbd "C-,"))

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
