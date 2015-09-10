(require-packages '(helm))

(after-load 'helm
  (setq helm-idle-delay 0.01
        helm-input-idle-delay 0.01
        helm-completion-mode-string ""
        helm-full-frame nil
        helm-buffers-fuzzy-matching t
        helm-split-window-default-side 'right)
  (define-key helm-map (kbd right-little-finger-key) 'helm-maybe-exit-minibuffer))

(after-load 'helm-files
  ;; Show full file-path in helm result
  (setq helm-ff-transformer-show-only-basename nil))


(require 'helm-config)
(helm-mode 1)

(provide 'init-helm)
