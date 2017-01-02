;; key bindings
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C--") 'undo)
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)
(global-set-key (kbd "<backspace>") 'backward-delete-char)
(global-set-key (kbd "M-j") 'delete-indentation)

(global-set-key (kbd "C-x g") 'beginning-of-buffer)
(global-set-key (kbd "C-x G") 'end-of-buffer)

(defun ido-common-bind-key ()
  (define-key ido-common-completion-map (kbd right-little-finger-key) 'ido-exit-minibuffer)
  (define-key ido-common-completion-map (kbd "SPC") 'ido-next-match)
  (define-key ido-common-completion-map (kbd ",") 'ido-prev-match))

(after-load 'ido
  (add-hook 'ido-minibuffer-setup-hook 'ido-common-bind-key))

;; smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd (concat "C-" right-little-finger-key)) 'smex)

;; find a file
(global-set-key (kbd "C-x C-f") 'helm-find-file)
;; find a dir
(global-set-key (kbd "C-x C-d") 'helm-find-dir)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(global-set-key (kbd "M-i") 'evil-visual-line)
(global-set-key (kbd "C-w") 'smart-kill-region)
(global-set-key (kbd "M-w") 'smart-kill-ring-save)
(global-set-key (kbd "C-y") 'evil-paste-after)
(global-set-key (kbd "C-x Y") 'evil-paste-before)

;; iedit
(global-set-key (kbd "C-,") 'iedit-mode)
(setq iedit-toggle-key-default (kbd "C-,"))

(define-key evil-visual-state-map (kbd "C-g") (lambda()(interactive)(evil-local-mode -1)))

(after-load 'helm
  (define-key helm-map (kbd right-little-finger-key) 'helm-maybe-exit-minibuffer))

;; switch buffer
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
(global-set-key (kbd right-little-finger-key) 'persp-switch-to-buffer)
(global-set-key (kbd "C-x C-b") 'helm-mini)

(global-set-key (kbd "C-c f r") 'buffer-file-run)

;; jump to bookmarked buffer
(global-set-key (kbd "C-x r b") 'helm-bookmarks)

(global-set-key (kbd "C-x K") 'kill-this-buffer)

(define-key emacs-lisp-mode-map (kbd "M-.") 'elisp-find-function-under-point)

(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g l") (lambda () (interactive) (magit-log '("HEAD"))))
(global-set-key (kbd "C-c g d") 'magit-diff-popup)
(global-set-key (kbd "C-c g b") 'magit-branch-popup)
(global-set-key (kbd "C-c g P") 'magit-pull-popup)

(after-load 'magit
  (define-key magit-status-mode-map (kbd "C-c C-a") 'magit-just-amend)
  (define-key magit-log-mode-map (kbd "g") 'beginning-of-buffer)
  (define-key magit-log-mode-map (kbd "G") 'end-of-buffer))

(after-load 'company
  (define-key company-active-map (kbd right-little-finger-key) 'company-complete-selection)
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort))

(after-load 'dired
  (add-hook 'dired-mode-hook
            (lambda ()
              (local-set-key (kbd "b") 'scroll-down-command)
              (local-set-key (kbd " ") 'scroll-up-command)))
  (define-key dired-mode-map (kbd "C-c m s") 'dired-mark-source-file)
  (define-key dired-mode-map (kbd "C-c m d") 'dired-mark-destination-dir)
  (define-key dired-mode-map (kbd "c") 'dired-copy-file-by-register)
  (define-key dired-mode-map (kbd "C-c r s") 'dired-read-source-file)
  (define-key dired-mode-map (kbd "C-c r d") 'dired-read-destination-dir)
  (define-key dired-mode-map (kbd "C-o") 'other-window))

(global-set-key (kbd "C-c d") 'dired-here)


(after-load 'yasnippet
  (define-key yas-minor-mode-map [backtab] nil)
  (define-key yas-minor-mode-map [(tab)]        nil)
  (define-key yas-minor-mode-map (kbd "TAB")    nil)
  (define-key yas-minor-mode-map (kbd "S-SPC")  'helm-yas-complete))


;; helm-gtags
(after-load 'helm-gtags
  ;; key bindings
  (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
  (define-key helm-gtags-mode-map (kbd "C-j") 'helm-code-select)
  (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
  (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))

(define-key gud-mode-map (kbd "C-q") 'project-debug-quit)


;; projectile
(after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p g") 'helm-projectile-grep)
  (define-key projectile-mode-map (kbd "C-c p R") 'project-update-tags)
  (define-key projectile-mode-map (kbd "C-c p c") 'project-compile)
  (define-key projectile-mode-map (kbd "C-c p r") 'project-run)
  (define-key projectile-mode-map (kbd "C-c p d") 'project-debug))


(after-load 'cc-mode
  (add-hook 'c-mode-common-hook
            (lambda ()
              (local-set-key (kbd "C-i") 'clang-format))))

(after-load 'cc-mode
  (dolist (mode-hook '(c-mode-hook c++-mode-hook))
    (add-hook mode-hook
              (lambda ()
                (local-set-key  (kbd "C-c o") 'cc-switch-source-header-file)))))

(defun eshell-set-keybindings ()
  (define-key eshell-mode-map (kbd "C-r")
    (lambda ()
      (interactive)
      (let (
            (helm-split-window-default-side 'below))
        (recenter)
        (helm-eshell-history))))
  (define-key eshell-mode-map (kbd "C-j")
    (lambda ()
      (interactive)
      (let (
            (helm-split-window-default-side 'below))
        (recenter)
        (helm-eshell-dir-history))))
  (define-key eshell-mode-map (kbd "C-w") 'eshell-kill-input))

(add-hook 'eshell-mode-hook 'eshell-set-keybindings)

(global-set-key (kbd "C-c e") 'eshell-here)

(after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "q") 'org-agenda-bury))

(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)

(after-load 'org
  (define-key org-mode-map (kbd "<C-tab>") nil))

(after-load 'view
  (define-key view-mode-map (kbd "G") 'end-of-buffer)
  (define-key view-mode-map (kbd "v") 'View-scroll-page-backward)
  (define-key view-mode-map (kbd "J") 'scroll-up-line)
  (define-key view-mode-map (kbd "K") 'scroll-down-line)
  (define-key view-mode-map (kbd "h") 'backward-char)
  (define-key view-mode-map (kbd "j") 'next-line)
  (define-key view-mode-map (kbd "k") 'previous-line)
  (define-key view-mode-map (kbd "l") 'forward-char)
  (define-key view-mode-map (kbd "o") 'other-window)
  (define-key view-mode-map (kbd "a") 'move-beginning-of-line)
  (define-key view-mode-map (kbd "e") 'move-end-of-line)
  (define-key view-mode-map (kbd "w") 'kill-ring-save)
  (define-key view-mode-map (kbd "f") 'forward-word)
  (define-key view-mode-map (kbd "b") 'backward-word)
  (define-key view-mode-map (kbd "E") 'my-View-exit)
  (define-key view-mode-map (kbd "q") 'bury-buffer))

(global-set-key (kbd "C-c t") 'term-here)

(global-set-key (kbd "C-c s c") 'session-create)
(global-set-key (kbd "C-c s s") 'session-switch)
(global-set-key (kbd "C-c s k") 'session-kill)
(global-set-key (kbd "C-c s r") 'session-rename)
(global-set-key (kbd "C-c s n") 'session-next)
(global-set-key (kbd "C-c s p") 'session-previous)
(global-set-key (kbd "C-c s a") 'session-add-buffer)
(global-set-key (kbd "C-c s <tab>") 'session-last)
(global-set-key (kbd (concat "M-" right-little-finger-key)) 'session-switch)
(global-set-key (kbd "M-o") 'session-next)
(global-set-key (kbd "M-n") 'session-next)
(global-set-key (kbd "M-p") 'session-previous)
(global-set-key (kbd "<C-tab>") 'session-last)

(global-set-key (kbd "C-S-SPC")
                (lambda ()
                  (interactive)
                  (require 'chinese-pyim)
                  (toggle-input-method)))

(global-set-key (kbd "C-S-s") 'isearch-forward-pinyin)
(global-set-key (kbd "C-S-r") 'isearch-backward-pinyin)

(after-load 'chinese-pyim-core
  (define-key pyim-mode-map (kbd ".") 'pyim-next-page)
  (define-key pyim-mode-map (kbd ",") 'pyim-previous-page))

(define-key term-raw-map (kbd "C-o") 'other-window)
(define-key term-raw-map (kbd "<prior>") 'scroll-down-command)
(define-key term-raw-map (kbd "<next>") 'scroll-up-command)
;;(define-key term-raw-map (kbd "C-h k") 'describe-key)
(define-key term-raw-map (kbd "<home>") 'beginning-of-buffer)
(define-key term-raw-map (kbd "<end>") 'end-of-buffer)
(define-key term-raw-map (kbd "M-x") 'smex)
(define-key term-raw-map (kbd "C-v") 'scroll-up-command)
;; sometimes we need to use vi in term-mode. so we need to make esc work
(define-key term-raw-map (kbd "<escape>") 'term-send-esc)
;; bind C-n/p to behave the same as ordinary term
(define-key term-raw-map (kbd "C-n") 'term-send-down)
(define-key term-raw-map (kbd "C-p") 'term-send-up)

;; key map
(define-key function-key-map "\e[24~" [f5])

(delete '("M-o" . term-send-backspace) term-bind-key-alist)

;; bind C-r to search shell command history, and M-r to search buffer
(delete '("M-r" . term-send-reverse-search-history) term-bind-key-alist)
(delete '("C-r" . isearch-backward) term-bind-key-alist)
(add-to-list 'term-bind-key-alist '("M-r" . isearch-backward))
(add-to-list 'term-bind-key-alist '("C-r" . term-send-reverse-search-history))

(define-key term-mode-map (kbd "M-SPC") 'term-toggle-submode)
(define-key term-raw-map (kbd "M-SPC") 'term-toggle-submode)

(after-load 'python
  (define-key python-mode-map (kbd "M-.") 'jedi:goto-definition)
  (define-key python-mode-map (kbd "M-,") 'jedi:goto-definition-pop-marker))

(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-j") 'helm-code-select)
            (local-set-key (kbd "C-c <") 'helm-gtags-previous-history)
            (local-set-key (kbd "C-c >") 'helm-gtags-next-history)))

(after-load 'doc-view
  (define-key doc-view-mode-map (kbd "g") 'doc-view-first-page)
  (define-key doc-view-mode-map (kbd "G") 'doc-view-last-page)
  (define-key doc-view-mode-map (kbd "v") 'doc-view-scroll-down-or-previous-page)
  (define-key doc-view-mode-map (kbd "C-c g") 'doc-view-goto-page)
  (define-key doc-view-mode-map (kbd "m") 'doc-view-toggle-modeline))

(provide 'init-keybinding)
