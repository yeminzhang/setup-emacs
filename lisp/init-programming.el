(require-packages '(helm-gtags yasnippet helm-c-yasnippet highlight-parentheses expand-region linum-relative))

(add-hook 'prog-mode-hook 'configure-programming-buffer-common)

(defun configure-programming-buffer-common ()
  (setq show-trailing-whitespace t)
  (linum-relative-mode t)
  (linum-mode t)
  (highlight-parentheses-mode t)
  (local-set-key (kbd "C-.") 'er/expand-region))

(after-load 'linum-relative
  (setq linum-relative-plusp-offset 1
        linum-relative-current-symbol ""))

(defun readelf (filename)
  (interactive (list (ido-read-file-name "file: ")))
  (shell-command (concat "readelf -a " filename)))

;; expand region
(setq expand-region-contract-fast-key ","
      expand-region-reset-fast-key "r")

;; highlight-parentheses
(after-load 'highlight-parentheses
  (setq hl-paren-delay 0.2
        hl-paren-colors '("Springgreen3"
                          "IndianRed1"
                          "IndianRed3"
                          "IndianRed4")))

;; Package: yasnippet
(after-load 'yasnippet
  (yas-reload-all))

(autoload 'yas-minor-mode-on "yasnippet")

(after-load 'company
  (push '(company-semantic :with company-yasnippet) company-backends))

;; which function mode
(which-function-mode t)
(setq which-func-modes '(emacs-lisp-mode c++-mode c-mode))

;; configure semantic
(semantic-mode 1)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

;; helm-gtags
(after-load 'helm-gtags
  (setq
   helm-gtags-ignore-case t
   helm-gtags-auto-update t
   helm-gtags-use-input-at-cursor t
   helm-gtags-pulse-at-cursor t
   helm-gtags-prefix-key "\C-cg"
   helm-gtags-suggested-key-mapping t
   helm-gtags-mode-name " HG"))

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
