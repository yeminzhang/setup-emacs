;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-strip-common-suffix t)

;; ibuffer
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
;;(add-hook 'ibuffer-mode-hook #'(lambda () (hl-line-mode 1)))

;; ido
(require 'ido)
(global-set-key (kbd "ö") 'ido-switch-buffer)

;; jump to bookmarked buffer
(global-set-key (kbd "C-x r b")
  (lambda() (interactive)
    (helm
     :prompt "Switch to: "
     :candidate-number-limit 25                 ;; up to 25 of each
     :sources
     '(
       helm-source-bookmarks
        ))))


(global-set-key (kbd "C-x K") 'kill-this-buffer)

;; desktop save
(desktop-save-mode 1)
(setq desktop-restore-eager 30)
;;(add-to-list 'desktop-modes-not-to-save 'help-mode)
;;(add-to-list 'desktop-modes-not-to-save 'man-mode)

(add-hook 'window-configuration-change-hook
'check-revert-file)

;; force all buffers to be displayed in the same window
;;(setq same-window-buffer-names '("*eshell*"))
(setq pop-up-windows nil)
(setq Man-notify-method 'pushy)

(defun check-revert-file ()
(if (buffer-file-name)
(unless (verify-visited-file-modtime (current-buffer))
;;(unless (buffer-modified-p) (revert-buffer t t t))
(revert-buffer)))) ; ask for confirmation

(provide 'init-buffer)
