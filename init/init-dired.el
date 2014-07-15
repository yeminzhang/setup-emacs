
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

;;Automatically revert buffer every 3 seconds
(add-hook 'dired-mode-hook #'(lambda () (auto-revert-mode 1)))
;;(setq auto-revert-interval 3)
;;(setq dired-auto-revert-buffer t)

(define-key dired-mode-map (kbd "C-c m s") 'dired-mark-source-file)
(define-key dired-mode-map (kbd "C-c m d") 'dired-mark-destination-dir)
(define-key dired-mode-map (kbd "c") 'dired-copy-file-by-register)
(define-key dired-mode-map (kbd "C-c r s") 'dired-read-source-file)
(define-key dired-mode-map (kbd "C-c r d") 'dired-read-destination-dir)

(defun dired-mark-source-file()
(interactive)
(progn
(set-register 121 (dired-filename-at-point))
(message (concat "Src file: " (dired-filename-at-point)))))

(defun dired-mark-destination-dir()
(interactive)
(progn
(set-register 122 dired-directory)
(message (concat "Dest dir: " dired-directory))))

(defun dired-copy-file-by-register()
(interactive)
(progn
(setq src-file (get-register 121))
(setq dest-dir (get-register 122))
(dired-copy-file src-file dest-dir t)))

(defun dired-read-source-file()
(interactive)
(message (concat "Src file: " (get-register 121))))

(defun dired-read-destination-dir()
(interactive)
(message (concat "Dest dir: " (get-register 122))))

;; tramp
(require 'tramp)
(setq password-cache t)
(setq password-cache-expiry nil)
(setq tramp-default-method "ssh")
(setq tramp-default-user nil)
(setq recentf-auto-cleanup 'never)
