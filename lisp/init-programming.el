(use-package linum-relative
  :ensure t
  :defer t
  :diminish linum-relative-mode
  :config
  (setq linum-relative-plusp-offset 1
        linum-relative-current-symbol ""))

;; highlight-parentheses
(use-package highlight-parentheses
  :ensure t
  :defer t
  :config
  (setq hl-paren-delay 0.2
        hl-paren-colors '("Springgreen3"
                          "IndianRed1"
                          "IndianRed3"
                          "IndianRed4")))

(use-package prog-mode
  :defer t
  :config
  (add-hook 'prog-mode-hook 'configure-programming-buffer-common)

  (defun configure-programming-buffer-common ()
    (setq show-trailing-whitespace t)
    (linum-relative-mode t)
    (linum-mode t)
    (highlight-parentheses-mode t)
    (local-set-key (kbd "C-.") 'er/expand-region)))

(defun readelf (filename)
  (interactive (list (ido-read-file-name "file: ")))
  (shell-command (concat "readelf -a " filename)))

;; expand region
(use-package expand-region
  :ensure t
  :defer t)
(setq expand-region-contract-fast-key ","
      expand-region-reset-fast-key "r")

;; Package: yasnippet
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode-on
  :config
  (yas-reload-all))

(use-package helm-c-yasnippet
  :ensure t
  :defer t)

;; TODO investigate how to use ycmd for auto complete
(use-package company
  :defer t
  :config
  (push '(company-semantic :with company-yasnippet) company-backends))

;; which function mode
(which-function-mode t)
(use-package which-func
  :defer t
  :config
  (setq which-func-modes '(emacs-lisp-mode c++-mode c-mode)))

;; configure semantic
;; TODO investigate how to use ycmd for auto complete
(semantic-mode 1)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

;; helm-gtags
(use-package helm-gtags
  :ensure t
  :defer t
  :diminish helm-gtags-mode
  :config
  (setq
   helm-gtags-ignore-case t
   helm-gtags-auto-update t
   helm-gtags-use-input-at-cursor t
   helm-gtags-pulse-at-cursor t
;;   helm-gtags-prefix-key "\C-cg"
   helm-gtags-suggested-key-mapping t))

(defun helm-code-select ()
  (interactive)
  (require 'helm-semantic)
  (require 'helm-gtags)
  (helm
   :prompt "Go to: "
   :candidate-number-limit 9999
   :sources
   '(
     helm-source-semantic
     helm-source-gtags-select)))

(defun format-buffer ()
  (interactive)
  (when c-buffer-is-cc-mode (cc-format-buffer)))

(provide 'init-programming)
