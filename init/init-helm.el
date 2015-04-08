(require-packages '(helm))
(require 'helm-config)
(helm-mode 1)
(setq helm-idle-delay 0.01)
(setq helm-input-idle-delay 0.01)
(define-key helm-map (kbd right-little-finger-key) 'helm-maybe-exit-minibuffer)
(setq helm-completion-mode-string "")
(setq helm-full-frame nil)
(setq helm-buffers-fuzzy-matching t)

(provide 'init-helm)
