(require-packages '(dired+))
;; dired+
(require 'dired+)
(setq diredp-hide-details-initially-flag nil)
(setq diredp-hide-details-propagate-flag nil)

;; dired
(setq dired-listing-switches "-lhaD")
(setq dired-isearch-filenames t)
(setq dired-recursive-deletes 1)
(setq delete-by-moving-to-trash t)
(setq dired-recursive-copies "always")
;; underline the current line in dired mode
(add-hook 'dired-mode-hook #'(lambda () (hl-line-mode 1)))
(add-hook 'dired-mode-hook #'(lambda () (local-set-key (kbd "b") 'scroll-down-command)))
(add-hook 'dired-mode-hook #'(lambda () (local-set-key (kbd " ") 'scroll-up-command)))
(add-hook 'dired-mode-hook #'(lambda () (rename-buffer (concat "d-" dired-directory))))

;;Automatically revert buffer every 2 seconds
(add-hook 'dired-mode-hook #'(lambda () (auto-revert-mode 1)))
(setq auto-revert-interval 2)
(setq dired-auto-revert-buffer t)

(define-key dired-mode-map (kbd "C-c m s") 'dired-mark-source-file)
(define-key dired-mode-map (kbd "C-c m d") 'dired-mark-destination-dir)
(define-key dired-mode-map (kbd "c") 'dired-copy-file-by-register)
(define-key dired-mode-map (kbd "C-c r s") 'dired-read-source-file)
(define-key dired-mode-map (kbd "C-c r d") 'dired-read-destination-dir)

(define-key dired-mode-map (kbd "C-o") 'other-window)

(defun dired-remove-tramp-method (path)
  (if (string-prefix-p "/" path) (substring path (+ 1 (s-index-of ":" path))) path))

(defun dired-mark-source-file()
(interactive)
(let* ((filename-at-point (dired-remove-tramp-method (dired-filename-at-point))))
	   (set-register 121 filename-at-point)
	   (message (concat "Src file: " filename-at-point))))

(defun dired-mark-destination-dir()
(interactive)
(let* ((dest-dir (dired-remove-tramp-method dired-directory)))
(set-register 122 dest-dir)
(message (concat "Dest dir: " dest-dir))))

(defun scp-copy-file (src-file dest-dir)
  ;;  (apply 'call-process "scp" nil nil nil (list src-file dest-dir)))
(switch-to-buffer eshell-buffer-name)
(end-of-buffer)
(eshell-kill-input)
(insert (concat "scp -3 " src-file " " dest-dir))
(eshell-send-input))


(defun dired-copy-file-by-register()
(interactive)
(let* (
(src-file (get-register 121))
(dest-dir (get-register 122)))
(with-current-buffer eshell-buffer-name
(end-of-buffer)
(eshell-kill-input)
(insert (concat "scp -3 " src-file " " dest-dir))
(message (concat "Copying from " src-file " to " dest-dir))
(eshell-send-input))))

(defun dired-read-source-file()
(interactive)
(message (concat "Src file: " (get-register 121))))

(defun dired-read-destination-dir()
(interactive)
(message (concat "Dest dir: " (get-register 122))))

(provide 'init-dired)
