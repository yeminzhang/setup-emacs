;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-strip-common-suffix t)

;; switch buffer
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
(key-chord-define-global "jj" 'ido-switch-buffer)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(fset 'switch-to-previous-buffer
      (lambda (&optional arg)
        "Same as C-x b right-little-finger-key"
        (interactive "p")
        (kmacro-exec-ring-item (quote ([24 98 246] "b" right-little-finger-key)) arg)))
(key-chord-define-global "JJ" 'switch-to-previous-buffer)

;; This is a patch to prevent helm from sorting the buffer
;; list when narrowing
(defun helm-buffers-sort-transformer (candidates _source)
  candidates)

;; jump to bookmarked buffer
(global-set-key (kbd "C-x r b") 'helm-bookmarks)

(global-set-key (kbd "C-x K") 'kill-this-buffer)

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

(setq enable-local-variables :all)

(defun set-display-buffer-other-window (regex)
  (add-to-list 'display-buffer-alist
               `(,regex
                 (display-buffer-reuse-window
                  display-buffer-pop-up-window)
                 (reusable-frames . visible))
               ))

(provide 'init-buffer)
