(setq pop-up-windows nil)
(defun maybe-split-window (&optional switch-window)
  (when (one-window-p t)
    (split-window-horizontally))
  (when switch-window) (other-window 1))

(defun set-display-buffer-other-window (regex)
  (add-to-list 'display-buffer-alist
               `(,regex
                 (display-buffer-reuse-window
                  display-buffer-pop-up-window)
                 (reusable-frames . visible))))

(use-package ediff-wind
  :defer t
  :config
  (setq ediff-split-window-function 'split-window-horizontally))

(use-package window-numbering
  :ensure t
  :defer t)
(window-numbering-mode t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(provide 'init-window)
