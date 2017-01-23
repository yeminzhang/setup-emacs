;; uniquify
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-strip-common-suffix t))

(global-auto-revert-mode 1)

;; now only elisp buffer file is supported
(defun buffer-file-run ()
  (interactive)
  (when (eq major-mode 'emacs-lisp-mode) (elisp-save-and-eval-buffer)))

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
