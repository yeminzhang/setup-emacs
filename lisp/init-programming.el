(require-packages '(helm-gtags yasnippet helm-c-yasnippet ))

(dolist (hook '(c-mode-common-hook python-mode-hook emacs-lisp-mode-hook erlang-mode-hook sh-mode-hook))
  (add-hook hook (lambda () (setq show-trailing-whitespace t))))

(defun readelf (filename)
  (interactive (list (ido-read-file-name "file: ")))
  (shell-command (concat "readelf -a " filename)))

;; Package: yasnippet
(after-load 'yasnippet
  (yas-reload-all)
  (define-key yas-minor-mode-map [backtab] nil)
  (define-key yas-minor-mode-map [(tab)]        nil)
  (define-key yas-minor-mode-map (kbd "TAB")    nil)
  (define-key yas-minor-mode-map (kbd "S-SPC")  'helm-yas-complete))

(autoload 'yas-minor-mode-on "yasnippet")

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
   helm-gtags-mode-name " HG"
   )
  ;; key bindings
  (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
  (define-key helm-gtags-mode-map (kbd "C-j") 'helm-code-select)
  (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
  (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))

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
