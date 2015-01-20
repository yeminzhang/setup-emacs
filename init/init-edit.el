;;(require 'helm-config)

;; key bindings
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C--") 'undo)
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

;; Tabsn
(setq-default indent-tabs-mode 1)
(setq-default tab-width 8)
(setq-default c-basic-offset 8)

;; vim commands for convenience
;;(fset 'delete-whole-line "\C-a\C-k\C-k")
;;(global-set-key (kbd "C-m dd") 'delete-whole-line)
(global-set-key (kbd "C-x g") 'beginning-of-buffer)
(global-set-key (kbd "C-x G") 'end-of-buffer)
;;(fset 'copy-whole-line "\C-a\C- \C-e\M-w")
;;(global-set-key (kbd "C-x y") 'copy-whole-line)

(setq x-select-enable-clipboard t)

;; ido
(require 'ido)
(ido-mode 'both)
(setq ido-enable-flex-matching t)
(setq ido-enable-dot-prefix t)
(setq ido-enable-regexp nil)

(if (and (boundp 'keyboard-layout) (string= keyboard-layout "sv"))
(setq ido-exit-minibuffer-key "\366")
(setq ido-exit-minibuffer-key ";"))
(add-hook 'ido-minibuffer-setup-hook #'(lambda () (local-set-key (kbd ido-exit-minibuffer-key) 'ido-exit-minibuffer)))

;; smex
(add-to-list 'load-path (concat emacs-configuration-root-dir "smex"))
(require 'smex)
(global-set-key (kbd "M-x") 'smex)

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
     :prompt "Switch to: "
     :candidate-number-limit 25                 ;; up to 25 of each
     :sources
     '(
	   helm-source-files-in-current-dir ;; current dir
		helm-c-source-recentf               ;; recent files
;;	   helm-source-find-files
        helm-source-locate))))            ;; use 'locate'

(setq helm-dir-db-file (concat emacs-configuration-root-dir "allfolder"))


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
                (find-file candidate))))
  "Helm source for Go to Directory.")

(defun my-helm-find-dir ()
(interactive)
(helm
   :prompt "Go to dir: "
   :candidate-number-limit 25                 ;; up to 25 of each
   :sources
   '(
	 my-helm-source-find-dir
 )))

(global-set-key (kbd "C-x d") 'my-helm-find-dir)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(defun updatedb ()
(interactive)
(if (boundp 'updatedb-options)
(call-process-shell-command (concat "updatedb " updatedb-options) nil 0)))


(defun updatedir-db ()
(interactive)
(shell-command (concat "find / -type d 2>/dev/null 1>" helm-dir-db-file)))

(unless (boundp 'updatedb-timer)
(run-with-idle-timer 10800 t 'updatedb)
(setq updatedb-timer t))

(defun insert-special-char (char_str)
(interactive (list (ido-completing-read "Char to insert: " (list "ö" "ä" "å" "Ö" "Ä" "Å" "~"))))
(insert char_str))

(add-to-list 'desktop-globals-to-save 'kill-ring)

(defun show-trailing-whitespace ()
(setq show-trailing-whitespace t))

(add-hook 'c++-mode-hook 'show-trailing-whitespace)

(defun my-kill-ring-save ()
  "When called interactively with no active region, copy a single line without \n."
  (interactive (if mark-active (kill-ring-save (region-beginning) (region-end))
                 (message "Copied line")
                 (kill-ring-save (line-beginning-position)
                       (line-end-position)))))

(defun my-kill-region ()
  "When called interactively with no active region, kill a single line without \n."
  (interactive)
  (if mark-active (kill-region (region-beginning) (region-end))
    (progn (move-beginning-of-line nil) (kill-line) (backward-delete-char 1))))

(global-set-key (kbd "C-w") 'my-kill-region)
(global-set-key (kbd "M-w") 'my-kill-ring-save)
(global-set-key (kbd "C-x y") #'(lambda ()(interactive) (move-end-of-line nil) (newline) (yank)))
(global-set-key (kbd "C-x Y") #'(lambda ()(interactive) (move-beginning-of-line nil) (yank) (newline) (backward-char 1)))

;; undo-tree
(add-to-list 'load-path (concat emacs-configuration-root-dir "undo-tree"))
(require 'undo-tree)
(global-undo-tree-mode)

(set-language-environment "UTF-8")

(provide 'init-edit)
