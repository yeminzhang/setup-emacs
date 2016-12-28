;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-strip-common-suffix t)

;; This is a patch to prevent helm from sorting the buffer
;; list when narrowing
(defun helm-buffers-sort-transformer (candidates _source)
  candidates)

(global-auto-revert-mode 1)

;; force all buffers to be displayed in the same window
;;(setq same-window-buffer-names '("*eshell*"))
(setq pop-up-windows nil)
(setq Man-notify-method 'pushy)

(setq enable-local-variables :all)

(defun set-display-buffer-other-window (regex)
  (add-to-list 'display-buffer-alist
               `(,regex
                 (display-buffer-reuse-window
                  display-buffer-pop-up-window)
                 (reusable-frames . visible))))

(provide 'init-buffer)
