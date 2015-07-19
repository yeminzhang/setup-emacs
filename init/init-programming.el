(require-packages '(helm-gtags yasnippet helm-c-yasnippet ))

(defun readelf (filename)
  (interactive (list (ido-read-file-name "file: ")))
  (shell-command (concat "readelf -a " filename)))

;; configure semantic
(require 'semantic)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)
(require 'helm-semantic)

(defun get-mode-map (mode)
  (if (eq mode 'cc-mode) 'c++-mode-map)
  )

;; Package: yasnippet
(require 'yasnippet)
(yas-global-mode 1)

(define-key yas-minor-mode-map [backtab] 'yas-expand)

(define-key yas-minor-mode-map [(tab)]        nil)
(define-key yas-minor-mode-map (kbd "TAB")    nil)
(define-key yas-minor-mode-map (kbd "<tab>")  nil)

(require 'helm-gtags)

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 helm-gtags-mode-name " HG"
 )

;; Enable helm-gtags-mode in Dired so you can jump to any tag
;; when navigate project tree with Dired
;; Enable helm-gtags-mode in Eshell for the same reason as above
;; Enable helm-gtags-mode in languages that GNU Global supports
(dolist (hook '(c-mode-hook c++-mode-hook java-mode-hook dired-mode-hook eshell-mode-hook))
  (add-hook hook 'helm-gtags-mode))

;; key bindings
(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-code-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

(defun helm-code-select ()
  (interactive)
  (helm
   :prompt "Go to: "
   :candidate-number-limit 9999
   :sources
   '(
	 helm-source-semantic
	 helm-source-gtags-select)))

(add-to-list 'ido-ignore-buffers "TAGS")

(provide 'init-programming)
