(require-packages '(smex undo-tree volatile-highlights iedit bookmark+ evil key-chord))

;; enable key-chord
(require 'key-chord)
(setq key-chord-one-key-delay 0.6
	  key-chord-two-keys-delay 0.5)
(key-chord-mode 1)

;; key bindings
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C--") 'undo)
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)
(global-set-key (kbd "<backspace>") 'backward-delete-char)

;; Tabsn
(setq-default indent-tabs-mode 1)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

;; whitespace
(setq-default show-trailing-whitespace t)
(require 'whitespace)
(setq whitespace-style '(tab-mark))  ;;turns on white space mode only for tabs
(global-whitespace-mode 1)

(global-set-key (kbd "C-x g") 'beginning-of-buffer)
(global-set-key (kbd "C-x G") 'end-of-buffer)

(setq x-select-enable-clipboard t)

;; ido
(require 'ido)
(ido-mode 'both)
(setq ido-enable-flex-matching t)
(setq ido-enable-dot-prefix t)
(setq ido-enable-regexp nil)
(setq ido-ignore-extensions nil)
(add-hook 'ido-minibuffer-setup-hook 'ido-common-bind-key)

(defun ido-common-bind-key ()
  (define-key ido-common-completion-map (kbd right-little-finger-key) 'ido-exit-minibuffer)
  (define-key ido-common-completion-map (kbd "SPC") 'ido-next-match)
  (define-key ido-common-completion-map (kbd ",") 'ido-prev-match))

;; smex
(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd (concat "C-" right-little-finger-key)) 'smex)

;; auto update smex cache after load a file
(defun smex-update-after-load (unused)
  (when (boundp 'smex-cache)
	(smex-update)))

(add-hook 'after-load-functions 'smex-update-after-load)

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
					   str)
	(setq str (replace-match "" t t str)))
  str)

;; Show full file-path in helm result
(setq helm-ff-transformer-show-only-basename nil)

;; file a file
(global-set-key (kbd "C-x C-f")
				(lambda() (interactive)
				  (helm
				   :prompt "Open file: "
				   :candidate-number-limit 25                 ;; up to 25 of each
				   :sources
				   '(
					 helm-source-files-in-current-dir ;; current dir
					 helm-c-source-recentf               ;; recent files
					 helm-source-projectile-files-list
					 helm-source-locate))))            ;; use 'locate'

(setq helm-dir-db-file (expand-file-name "allfolder" user-emacs-directory))


(defvar my-helm-source-find-dir
  `((name . "Go to dir:")
	(init . (lambda ()
			  (with-current-buffer (helm-candidate-buffer 'global)
				(insert-file-contents helm-dir-db-file))))
	(candidates-in-buffer)
	(keymap . ,helm-generic-files-map)
	(filtered-candidate-transformer . (lambda (candidates sources)
										(reverse candidates)))
	(candidate-number-limit . 9999)
	(action . (lambda (candidate)
				(helm-find-file-or-marked candidate))))
  "Helm source for Go to Directory.")

(defun my-helm-find-dir ()
  (interactive)
  (helm
   :prompt "Go to dir: "
   :candidate-number-limit 25                 ;; up to 25 of each
   :sources
   '(
	 helm-source-projectile-directories-list
	 my-helm-source-find-dir
	 )))


(global-set-key (kbd "C-x d") 'my-helm-find-dir)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(setq locate-db-file "~/.mlocate.db")
;; By default regexp is not used. Add -r in a helm session to enable it
(if *is-linux*
	(setq helm-locate-command (concat "locate %s -d " locate-db-file " -e -A %s")))

(defun updatedb ()
  (interactive)
  (call-process-shell-command (concat "updatedb -o " locate-db-file " -l 0") nil 0))

(defun updatedir-db ()
  (interactive)
  (call-process-shell-command (concat "find / -type d 2>/dev/null 1>" helm-dir-db-file) nil 0))

;; updatedb every 30 minutes
(unless (boundp 'updatedb-timer)
  (run-with-timer 1800 1800 'updatedb)
  (run-with-timer 1800 1800 'updatedir-db)
  (setq updatedb-timer t))

(defun insert-special-char (char_str)
  (interactive (list (ido-completing-read "Char to insert: " (list "ö" "ä" "å" "Ö" "Ä" "Å" "~"))))
  (insert char_str))

(require 'desktop)
(add-to-list 'desktop-globals-to-save 'kill-ring)

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

(defcustom iedit-toggle-key-default (kbd "C-,")
  "If no-nil, the key is inserted into global-map, isearch-mode-map, esc-map and help-map."
  :type 'vector
  :group 'iedit)
(require 'iedit)

;; recentf
(setq recentf-max-menu-items 100
	  recentf-max-saved-items 200)

;; bookmark+
(require 'bookmark+)
(if (and (not bookmarks-already-loaded) (file-readable-p bookmark-default-file))
	(bookmark-load bookmark-default-file))

;; Auto save bookmark to file every 8 modifications
(setq bookmark-save-flag 8)

(set-language-environment "UTF-8")

(updatedb)
(updatedir-db)

(provide 'init-edit)
