(require-packages '(helm))

(after-load 'helm
  (setq helm-idle-delay 0.01
        helm-input-idle-delay 0.01
        helm-completion-mode-string ""
        helm-full-frame nil
        helm-buffers-fuzzy-matching t
        helm-split-window-default-side 'other))

(after-load 'helm-files
  ;; Show full file-path in helm result
  (setq helm-ff-transformer-show-only-basename nil))


(require 'helm-config)
(helm-mode 1)

(provide 'init-helm)
