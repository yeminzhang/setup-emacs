;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-strip-common-suffix t)

;; ibuffer
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
;;(add-hook 'ibuffer-mode-hook #'(lambda () (hl-line-mode 1)))

;; switch buffer
(global-set-key (kbd "C-x b") 'helm-mini)
(require 'key-chord)
(key-chord-define-global "jj" 'ido-switch-buffer)
(key-chord-define-global "JJ" 'switch-to-previous-buffer)
(key-chord-mode 1)

;; This is a patch to prevent helm from sorting the buffer
;; list when narrowing
(defun helm-buffers-sort-transformer (candidates _source)
  candidates)

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

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (car (helm-buffer-list))))

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

(provide 'init-buffer)
