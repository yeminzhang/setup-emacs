(require-packages '(helm))

(after-load 'helm
  (setq helm-idle-delay 0.01)
  (setq helm-input-idle-delay 0.01)
  (define-key helm-map (kbd right-little-finger-key) 'helm-maybe-exit-minibuffer)
  (setq helm-completion-mode-string "")
  (setq helm-full-frame nil)
  (setq helm-buffers-fuzzy-matching t))

(after-load 'helm-files
  ;; Show full file-path in helm result
  (setq helm-ff-transformer-show-only-basename nil))


(require 'helm-config)
(helm-mode 1)

(provide 'init-helm)
