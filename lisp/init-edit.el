(require-packages '(smex undo-tree volatile-highlights iedit bookmark+ evil key-chord recentf-ext))

;; enable key-chord
(after-load 'key-chord
  (setq key-chord-one-key-delay 0.6
        key-chord-two-keys-delay 0.5))
(key-chord-mode 1)

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
(setq-default show-trailing-whitespace t)
(after-load 'whitespace
  (setq whitespace-style '(tab-mark)))  ;;turns on white space mode only for tabs

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
(global-set-key (kbd (concat "C-" right-little-finger-key)) 'smex)

;; auto update smex cache after load a file
(defun smex-update-after-load (unused)
  (when (boundp 'smex-cache)
    (smex-update)))

(add-hook 'after-load-functions 'smex-update-after-load)

(defvar my-helm-source-recentf
  `((name . "Recent Files")
    (init lambda nil
          (require 'recentf)
          (recentf-mode 1))
    (candidates lambda nil (delete-dups
                            (mapcar (lambda (file)
                                      (if (file-directory-p file) "" file))
                                    recentf-list)))
    (keymap keymap
            (23 . helm-yank-text-at-point)
            (67108925 . helm-ff-run-ediff-file)
            (3 keymap
               (64 . helm-ff-run-insert-org-link)
               (88 . helm-ff-run-open-file-with-default-tool)
               (24 . helm-ff-run-open-file-externally)
               (15 . helm-ff-run-switch-other-frame)
               (111 . helm-ff-run-switch-other-window)
               (61 . helm-ff-run-ediff-merge-file)
               (103 . helm-ff-run-gid))
            (27 keymap
                (46 . helm-ff-run-etags)
                (105 . helm-ff-properties-persistent)
                (68 . helm-ff-run-delete-file)
                (72 . helm-ff-run-hardlink-file)
                (83 . helm-ff-run-symlink-file)
                (76 . helm-ff-run-load-file)
                (66 . helm-ff-run-byte-compile-file)
                (67 . helm-ff-run-copy-file)
                (82 . helm-ff-run-rename-file)
                (103 keymap
                     (112 . helm-ff-run-pdfgrep)
                     (122 . helm-ff-run-zgrep)
                     (115 . helm-ff-run-grep)))
            (19 . helm-ff-run-grep)
            (29 . helm-ff-run-toggle-basename)
            keymap
            (246 . helm-maybe-exit-minibuffer)
            (f13 lambda nil
                 (interactive)
                 (helm-select-nth-action 12))
            (f12 lambda nil
                 (interactive)
                 (helm-select-nth-action 11))
            (f11 lambda nil
                 (interactive)
                 (helm-select-nth-action 10))
            (f10 lambda nil
                 (interactive)
                 (helm-select-nth-action 9))
            (f9 lambda nil
                (interactive)
                (helm-select-nth-action 8))
            (f8 lambda nil
                (interactive)
                (helm-select-nth-action 7))
            (f7 lambda nil
                (interactive)
                (helm-select-nth-action 6))
            (f6 lambda nil
                (interactive)
                (helm-select-nth-action 5))
            (f5 lambda nil
                (interactive)
                (helm-select-nth-action 4))
            (f4 lambda nil
                (interactive)
                (helm-select-nth-action 3))
            (f3 lambda nil
                (interactive)
                (helm-select-nth-action 2))
            (f2 lambda nil
                (interactive)
                (helm-select-nth-action 1))
            (menu-bar keymap
                      (help-menu keymap
                                 (describe keymap
                                           (describe-mode . helm-help))))
            (help keymap
                  (109 . helm-help))
            (f1 lambda nil
                (interactive)
                (helm-select-nth-action 0))
            (8 keymap
               (109 . helm-help)
               (104 . undefined)
               (8 . undefined)
               (4 . helm-enable-or-switch-to-debug))
            (20 . helm-toggle-resplit-and-swap-windows)
            (C-tab . undefined)
            (67108897 . helm-toggle-suspend-update)
            (3 keymap
               (63 . helm-help)
               (62 . helm-toggle-truncate-line)
               (21 . helm-refresh)
               (6 . helm-follow-mode)
               (9 . helm-copy-to-buffer)
               (11 . helm-kill-selection-and-quit)
               (25 . helm-yank-selection)
               (4 . helm-delete-current-selection)
               (45 . helm-swap-windows))
            (67108987 . helm-enlarge-window)
            (67108989 . helm-narrow-window)
            (19 . undefined)
            (18 . undefined)
            (23 . helm-yank-text-at-point)
            (24 keymap
                (2 . helm-resume-list-buffers-after-quit)
                (98 . helm-resume-previous-session-after-quit)
                (6 . helm-quit-and-find-file))
            (11 . helm-delete-minibuffer-contents)
            (67108896 . helm-toggle-visible-mark)
            (0 . helm-toggle-visible-mark)
            (C-M-up . helm-scroll-other-window-down)
            (C-M-down . helm-scroll-other-window)
            (M-prior . helm-scroll-other-window-down)
            (M-next . helm-scroll-other-window)
            (12 . helm-recenter-top-bottom-other-window)
            (15 . helm-next-source)
            (10 . helm-execute-persistent-action)
            (26 . helm-execute-persistent-action)
            (9 . helm-select-action)
            (13 . helm-maybe-exit-minibuffer)
            (left . helm-previous-source)
            (right . helm-next-source)
            (7 . helm-keyboard-quit)
            (22 . helm-next-page)
            (27 keymap
                (110 . next-history-element)
                (112 . previous-history-element)
                (115 . undefined)
                (5 . helm-display-all-sources)
                (1 . helm-show-all-in-this-source-only)
                (117 . helm-unmark-all)
                (97 . helm-mark-all)
                (109 . helm-toggle-all-marks)
                (41 . helm-next-visible-mark)
                (40 . helm-prev-visible-mark)
                (91)
                (32 . helm-toggle-visible-mark)
                (33554454 . helm-scroll-other-window-down)
                (25 . helm-scroll-other-window-down)
                (22 . helm-scroll-other-window)
                (12 . helm-reposition-window-other-window)
                (62 . helm-end-of-buffer)
                (60 . helm-beginning-of-buffer)
                (118 . helm-previous-page))
            (next . helm-next-page)
            (prior . helm-previous-page)
            (C-up . helm-follow-action-backward)
            (C-down . helm-follow-action-forward)
            (16 . helm-previous-line)
            (14 . helm-next-line)
            (up . helm-previous-line)
            (down . helm-next-line)
            keymap
            (18 . helm-minibuffer-history)
            (menu-bar keymap
                      (minibuf "Minibuf" keymap
                               (previous menu-item "Previous History Item" previous-history-element :help "Put previous minibuffer history element in the minibuffer")
                               (next menu-item "Next History Item" next-history-element :help "Put next minibuffer history element in the minibuffer")
                               (isearch-backward menu-item "Isearch History Backward" isearch-backward :help "Incrementally search minibuffer history backward")
                               (isearch-forward menu-item "Isearch History Forward" isearch-forward :help "Incrementally search minibuffer history forward")
                               (return menu-item "Enter" exit-minibuffer :key-sequence "" :help "Terminate input and exit minibuffer")
                               (quit menu-item "Quit" abort-recursive-edit :help "Abort input and exit minibuffer")
                               "Minibuf"))
            (10 . exit-minibuffer)
            (13 . exit-minibuffer)
            (7 . abort-recursive-edit)
            (C-tab . file-cache-minibuffer-complete)
            (9 . self-insert-command)
            (XF86Back . previous-history-element)
            (up . previous-history-element)
            (prior . previous-history-element)
            (XF86Forward . next-history-element)
            (down . next-history-element)
            (next . next-history-element)
            (27 keymap
                (114 . previous-matching-history-element)
                (115 . next-matching-history-element)
                (112 . previous-history-element)
                (110 . next-history-element)))
    (action . helm-type-file-actions)
    (help-message . helm-generic-file-help-message)
    (filtered-candidate-transformer helm-fuzzy-highlight-matches)
    (filter-one-by-one lambda
                       (c)
                       (if
                           (and helm-ff-transformer-show-only-basename
                                (not
                                 (consp c)))
                           (cons
                            (helm-basename c)
                            c)
                         c))
    (pattern-transformer . helm-recentf-pattern-transformer)
    (match helm-mm-exact-match helm-mm-match)
    (header-line . "C-j: helm-type-file-actions (keeping session)")
    (dont-plug helm-compile-source--multi-match helm-compile-source--persistent-help)
    (matchplugin)
    (match-part lambda
                (candidate)
                (if
                    (or helm-ff-transformer-show-only-basename helm-recentf--basename-flag)
                    (helm-basename candidate)
                  candidate))))

;; find a file
(global-set-key (kbd "C-x C-f")
                (lambda() (interactive)
                  (helm
                   :prompt "Open file: "
                   :candidate-number-limit 25                 ;; up to 25 of each
                   :sources
                   '(
                     helm-source-files-in-current-dir ;; current dir
                     my-helm-source-recentf               ;; recent files
                     helm-source-projectile-files-list
                     helm-source-locate))))            ;; use 'locate'

(setq helm-dir-db-file (expand-file-name "allfolder" user-emacs-directory))

(defvar my-helm-source-recentd
  `((name . "Recent Directories")
    (init lambda nil
          (require 'recentf)
          (recentf-mode 1))
    (candidates lambda nil (delete-dups
                            (mapcar (lambda (file)
                                      (if (file-directory-p file) file ""))
                                    recentf-list)))
    (keymap keymap
            (23 . helm-yank-text-at-point)
            (67108925 . helm-ff-run-ediff-file)
            (3 keymap
               (64 . helm-ff-run-insert-org-link)
               (88 . helm-ff-run-open-file-with-default-tool)
               (24 . helm-ff-run-open-file-externally)
               (15 . helm-ff-run-switch-other-frame)
               (111 . helm-ff-run-switch-other-window)
               (61 . helm-ff-run-ediff-merge-file)
               (103 . helm-ff-run-gid))
            (27 keymap
                (46 . helm-ff-run-etags)
                (105 . helm-ff-properties-persistent)
                (68 . helm-ff-run-delete-file)
                (72 . helm-ff-run-hardlink-file)
                (83 . helm-ff-run-symlink-file)
                (76 . helm-ff-run-load-file)
                (66 . helm-ff-run-byte-compile-file)
                (67 . helm-ff-run-copy-file)
                (82 . helm-ff-run-rename-file)
                (103 keymap
                     (112 . helm-ff-run-pdfgrep)
                     (122 . helm-ff-run-zgrep)
                     (115 . helm-ff-run-grep)))
            (19 . helm-ff-run-grep)
            (29 . helm-ff-run-toggle-basename)
            keymap
            (246 . helm-maybe-exit-minibuffer)
            (f13 lambda nil
                 (interactive)
                 (helm-select-nth-action 12))
            (f12 lambda nil
                 (interactive)
                 (helm-select-nth-action 11))
            (f11 lambda nil
                 (interactive)
                 (helm-select-nth-action 10))
            (f10 lambda nil
                 (interactive)
                 (helm-select-nth-action 9))
            (f9 lambda nil
                (interactive)
                (helm-select-nth-action 8))
            (f8 lambda nil
                (interactive)
                (helm-select-nth-action 7))
            (f7 lambda nil
                (interactive)
                (helm-select-nth-action 6))
            (f6 lambda nil
                (interactive)
                (helm-select-nth-action 5))
            (f5 lambda nil
                (interactive)
                (helm-select-nth-action 4))
            (f4 lambda nil
                (interactive)
                (helm-select-nth-action 3))
            (f3 lambda nil
                (interactive)
                (helm-select-nth-action 2))
            (f2 lambda nil
                (interactive)
                (helm-select-nth-action 1))
            (menu-bar keymap
                      (help-menu keymap
                                 (describe keymap
                                           (describe-mode . helm-help))))
            (help keymap
                  (109 . helm-help))
            (f1 lambda nil
                (interactive)
                (helm-select-nth-action 0))
            (8 keymap
               (109 . helm-help)
               (104 . undefined)
               (8 . undefined)
               (4 . helm-enable-or-switch-to-debug))
            (20 . helm-toggle-resplit-and-swap-windows)
            (C-tab . undefined)
            (67108897 . helm-toggle-suspend-update)
            (3 keymap
               (63 . helm-help)
               (62 . helm-toggle-truncate-line)
               (21 . helm-refresh)
               (6 . helm-follow-mode)
               (9 . helm-copy-to-buffer)
               (11 . helm-kill-selection-and-quit)
               (25 . helm-yank-selection)
               (4 . helm-delete-current-selection)
               (45 . helm-swap-windows))
            (67108987 . helm-enlarge-window)
            (67108989 . helm-narrow-window)
            (19 . undefined)
            (18 . undefined)
            (23 . helm-yank-text-at-point)
            (24 keymap
                (2 . helm-resume-list-buffers-after-quit)
                (98 . helm-resume-previous-session-after-quit)
                (6 . helm-quit-and-find-file))
            (11 . helm-delete-minibuffer-contents)
            (67108896 . helm-toggle-visible-mark)
            (0 . helm-toggle-visible-mark)
            (C-M-up . helm-scroll-other-window-down)
            (C-M-down . helm-scroll-other-window)
            (M-prior . helm-scroll-other-window-down)
            (M-next . helm-scroll-other-window)
            (12 . helm-recenter-top-bottom-other-window)
            (15 . helm-next-source)
            (10 . helm-execute-persistent-action)
            (26 . helm-execute-persistent-action)
            (9 . helm-select-action)
            (13 . helm-maybe-exit-minibuffer)
            (left . helm-previous-source)
            (right . helm-next-source)
            (7 . helm-keyboard-quit)
            (22 . helm-next-page)
            (27 keymap
                (110 . next-history-element)
                (112 . previous-history-element)
                (115 . undefined)
                (5 . helm-display-all-sources)
                (1 . helm-show-all-in-this-source-only)
                (117 . helm-unmark-all)
                (97 . helm-mark-all)
                (109 . helm-toggle-all-marks)
                (41 . helm-next-visible-mark)
                (40 . helm-prev-visible-mark)
                (91)
                (32 . helm-toggle-visible-mark)
                (33554454 . helm-scroll-other-window-down)
                (25 . helm-scroll-other-window-down)
                (22 . helm-scroll-other-window)
                (12 . helm-reposition-window-other-window)
                (62 . helm-end-of-buffer)
                (60 . helm-beginning-of-buffer)
                (118 . helm-previous-page))
            (next . helm-next-page)
            (prior . helm-previous-page)
            (C-up . helm-follow-action-backward)
            (C-down . helm-follow-action-forward)
            (16 . helm-previous-line)
            (14 . helm-next-line)
            (up . helm-previous-line)
            (down . helm-next-line)
            keymap
            (18 . helm-minibuffer-history)
            (menu-bar keymap
                      (minibuf "Minibuf" keymap
                               (previous menu-item "Previous History Item" previous-history-element :help "Put previous minibuffer history element in the minibuffer")
                               (next menu-item "Next History Item" next-history-element :help "Put next minibuffer history element in the minibuffer")
                               (isearch-backward menu-item "Isearch History Backward" isearch-backward :help "Incrementally search minibuffer history backward")
                               (isearch-forward menu-item "Isearch History Forward" isearch-forward :help "Incrementally search minibuffer history forward")
                               (return menu-item "Enter" exit-minibuffer :key-sequence "" :help "Terminate input and exit minibuffer")
                               (quit menu-item "Quit" abort-recursive-edit :help "Abort input and exit minibuffer")
                               "Minibuf"))
            (10 . exit-minibuffer)
            (13 . exit-minibuffer)
            (7 . abort-recursive-edit)
            (C-tab . file-cache-minibuffer-complete)
            (9 . self-insert-command)
            (XF86Back . previous-history-element)
            (up . previous-history-element)
            (prior . previous-history-element)
            (XF86Forward . next-history-element)
            (down . next-history-element)
            (next . next-history-element)
            (27 keymap
                (114 . previous-matching-history-element)
                (115 . next-matching-history-element)
                (112 . previous-history-element)
                (110 . next-history-element)))
    (action . helm-type-file-actions)
    (help-message . helm-generic-file-help-message)
    (filtered-candidate-transformer helm-fuzzy-highlight-matches)
    (filter-one-by-one lambda
                       (c)
                       (if
                           (and helm-ff-transformer-show-only-basename
                                (not
                                 (consp c)))
                           (cons
                            (helm-basename c)
                            c)
                         c))
    (pattern-transformer . helm-recentf-pattern-transformer)
    (match helm-mm-exact-match helm-mm-match)
    (header-line . "C-j: helm-type-file-actions (keeping session)")
    (dont-plug helm-compile-source--multi-match helm-compile-source--persistent-help)
    (matchplugin)
    (match-part lambda
                (candidate)
                (if
                    (or helm-ff-transformer-show-only-basename helm-recentf--basename-flag)
                    (helm-basename candidate)
                  candidate))))

(defvar my-helm-source-all-dirs
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
     my-helm-source-recentd
     my-helm-source-all-dirs
     )))

(global-set-key (kbd "C-x C-d") 'my-helm-find-dir)

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

(defcustom iedit-toggle-key-default (kbd "C-,")
  "If no-nil, the key is inserted into global-map, isearch-mode-map, esc-map and help-map."
  :type 'vector
  :group 'iedit)
(require 'iedit)

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
(updatedir-db)

(provide 'init-edit)
