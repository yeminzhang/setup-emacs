(require-packages '(dired+))

;; dired+
(after-load 'dired+
  (setq diredp-hide-details-initially-flag nil
        diredp-hide-details-propagate-flag nil))

;; dired
(after-load 'dired
  (setq dired-listing-switches "-lhaD"
        dired-isearch-filenames t
        dired-recursive-deletes 1
        delete-by-moving-to-trash t
        dired-recursive-copies "always"
        dired-auto-revert-buffer t)
  ;; underline the current line in dired mode
  (add-hook 'dired-mode-hook
            (lambda ()
              (hl-line-mode 1)
              (rename-buffer (concat "d-" dired-directory))
              ;;Automatically revert buffer every 2 seconds
              (auto-revert-mode 1))))

(defun dired-remove-tramp-method (path)
  (if (tramp-tramp-file-p path) (substring path (+ 1 (s-index-of ":" path))) path))

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

(defun dired-copy-file-by-register()
  (interactive)
  (let* (
         (src-file (get-register 121))
         (dest-dir (get-register 122)))
    (maybe-split-window t)
    (eshell)
    (eshell-run-command "cd")
    (message (concat "Copying from " src-file " to " dest-dir))
    (eshell-run-command (concat "scp -3 " src-file " " dest-dir))
    (other-window -1)))

(defun dired-read-source-file()
  (interactive)
  (message (concat "Src file: " (get-register 121))))

(defun dired-read-destination-dir()
  (interactive)
  (message (concat "Dest dir: " (get-register 122))))

(defun dired-here()
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                  default-directory)))
    (maybe-split-window t)
    (dired parent)))

(provide 'init-dired)
