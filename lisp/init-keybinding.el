
;; key bindings
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "<backspace>") 'backward-delete-char)
(global-set-key (kbd "M-j") 'delete-indentation)

(global-set-key (kbd "C-x g") 'beginning-of-buffer)
(global-set-key (kbd "C-x G") 'end-of-buffer)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(global-set-key (kbd "M-i") 'evil-visual-line)
(global-set-key (kbd "C-w") 'smart-kill-region)
(global-set-key (kbd "M-w") 'smart-kill-ring-save)
(global-set-key (kbd "C-y") 'evil-paste-after)
(global-set-key (kbd "C-x Y") 'evil-paste-before)

;; iedit
(use-package iedit-mode
  :bind ("C-," . iedit-mode)
  :config
  (setq iedit-toggle-key-default (kbd "C-,")))

(define-key evil-visual-state-map (kbd "C-g") (lambda()(interactive)(evil-local-mode -1)))

;; switch buffer
(use-package helm-buffers
  :bind ("C-x C-b" . helm-buffers-list))

(global-set-key (kbd "C-c f r") 'buffer-file-run)

;; jump to bookmarked buffer
(global-set-key (kbd "C-x r b") 'helm-bookmarks)

(global-set-key (kbd "C-x K") 'kill-this-buffer)

(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g l") (lambda () (interactive) (magit-log '("HEAD"))))
(global-set-key (kbd "C-c g d") 'magit-diff-popup)
(global-set-key (kbd "C-c g b") 'magit-branch-popup)
(global-set-key (kbd "C-c g P") 'magit-pull-popup)

(use-package magit-status
  :bind (:map magit-status-mode-map
              ("C-c C-a" . magit-just-amend))
  )

(use-package magit-log
  :bind (:map magit-log-mode-map
              ("g" . beginning-of-buffer)
              ("G" . end-of-buffer))
  )

(use-package company
  :bind (:map company-active-map
              ("C-n" . company-select-next-or-abort)
              ("C-p" . company-select-previous-or-abort))
  :config
  (define-key company-active-map (kbd ";") 'company-complete-selection))

(use-package dired
  :bind (:map dired-mode-map
              ("C-c m s" . dired-mark-source-file)
              ("C-c m d" . dired-mark-destination-dir)
              ("c" . dired-copy-file-by-register)
              ("C-c r s" . dired-read-source-file)
              ("C-c r d" . dired-read-destination-dir)
              ("C-o" . other-window))
  :config
  (add-hook 'dired-mode-hook
            (lambda ()
              (local-set-key (kbd "b") 'scroll-down-command)
              (local-set-key (kbd " ") 'scroll-up-command))))

(global-set-key (kbd "C-c d") 'dired-here)


(use-package yasnippet
  :defer t
  :config
  (define-key yas-minor-mode-map [backtab] nil)
  (define-key yas-minor-mode-map [(tab)]        nil)
  (define-key yas-minor-mode-map (kbd "TAB")    nil)
  (define-key yas-minor-mode-map (kbd "S-SPC")  'helm-yas-complete))


;; helm-gtags
(use-package helm-gtags
  :defer t
  :config
  ;; key bindings
  (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
  (define-key helm-gtags-mode-map (kbd "C-j") 'helm-code-select)
  (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
  (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))

(use-package gud
  :bind (:map gud-mode-map
              ("C-q" . project-debug-quit)))

;; projectile
(use-package projectile
  :defer t
  :config
  (define-key projectile-mode-map (kbd "C-c p g") 'helm-projectile-grep)
  (define-key projectile-mode-map (kbd "C-c p R") 'project-update-tags)
  (define-key projectile-mode-map (kbd "C-c p c") 'project-compile)
  (define-key projectile-mode-map (kbd "C-c p r") 'project-run)
  (define-key projectile-mode-map (kbd "C-c p d") 'project-debug))

(use-package cc-mode
  :defer t
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (local-set-key (kbd "C-i") 'clang-format)))

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

(use-package org-agenda
  :bind (:map org-agenda-mode-map
              ("q" . org-agenda-bury)))

(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)

(use-package org
  :defer t
  :config
  (define-key org-mode-map (kbd "<C-tab>") nil))

(use-package view
  :bind (:map view-mode-map
              ("G" . end-of-buffer)
              ("v" . View-scroll-page-backward)
              ("J" . scroll-up-line)
              ("K" . scroll-down-line)
              ("h" . backward-char)
              ("j" . next-line)
              ("k" . previous-line)
              ("l" . forward-char)
              ("o" . other-window)
              ("a" . move-beginning-of-line)
              ("e" . move-end-of-line)
              ("w" . kill-ring-save)
              ("f" . forward-word)
              ("b" . backward-word)
              ("E" . my-View-exit)
              ("q" . bury-buffer)))

(global-set-key (kbd "C-c t") 'term-here)

(global-set-key (kbd "C-c s s") 'session-switch)
(global-set-key (kbd "C-c s k") 'session-kill)
(global-set-key (kbd "C-c s r") 'session-rename)
(global-set-key (kbd "C-c s n") 'session-next)
(global-set-key (kbd "C-c s p") 'session-previous)
(global-set-key (kbd "C-c s f") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-c s <tab>") 'session-last)
(global-set-key (kbd "M-;") 'session-switch)
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

(use-package chinese-pyim-core
  :bind (:map pyim-mode-map
              ("." . pyim-page-next-page)
              ("," . pyim-page-previous-page)))

(use-package term
  :defer t
  :config
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

  (define-key term-mode-map (kbd "M-SPC") 'term-toggle-submode)
  (define-key term-raw-map (kbd "M-SPC") 'term-toggle-submode)
  ;; key map
  (define-key function-key-map "\e[24~" [f5]))


(use-package multi-term
  :defer t
  :config
  (delete '("M-o" . term-send-backspace) term-bind-key-alist)

  ;; bind C-r to search shell command history, and M-r to search buffer
  (delete '("M-r" . term-send-reverse-search-history) term-bind-key-alist)
  (delete '("C-r" . isearch-backward) term-bind-key-alist)
  (add-to-list 'term-bind-key-alist '("M-r" . isearch-backward))
  (add-to-list 'term-bind-key-alist '("C-r" . term-send-reverse-search-history))
  (delete '("C-n" . next-line) term-bind-key-alist)
  (delete '("C-p" . previous-line) term-bind-key-alist)
  (delete '("M-p" . term-send-up) term-bind-key-alist)
  (delete '("M-n" . term-send-down) term-bind-key-alist)
  (add-to-list 'term-bind-key-alist '("C-n" . term-send-down))
  (add-to-list 'term-bind-key-alist '("C-p" . term-send-up))
  )

(use-package python
  :defer t
  :config
  (define-key python-mode-map (kbd "M-.") 'jedi:goto-definition)
  (define-key python-mode-map (kbd "M-,") 'jedi:goto-definition-pop-marker)

  (add-hook 'python-mode-hook
            (lambda ()
              (local-set-key (kbd "C-j") 'helm-code-select)
              (local-set-key (kbd "C-c <") 'helm-gtags-previous-history)
              (local-set-key (kbd "C-c >") 'helm-gtags-next-history))))

(use-package doc-view
  :defer t
  :config
  (define-key doc-view-mode-map (kbd "g") 'doc-view-first-page)
  (define-key doc-view-mode-map (kbd "G") 'doc-view-last-page)
  (define-key doc-view-mode-map (kbd "v") 'doc-view-scroll-down-or-previous-page)
  (define-key doc-view-mode-map (kbd "C-c g") 'doc-view-goto-page)
  (define-key doc-view-mode-map (kbd "m") 'doc-view-toggle-modeline))

(provide 'init-keybinding)
