(require 'helm-config)
(helm-mode 1)
(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0.1)
(define-key helm-map (kbd "ö") 'helm-exit-minibuffer)
(setq helm-completion-mode-string "")
(setq helm-full-frame t)

(provide 'init-helm)
