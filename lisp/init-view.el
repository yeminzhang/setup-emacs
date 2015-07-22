;; doc view
(require 'doc-view)

;; view mode
(setq view-read-only t)

(require 'view)
(define-key view-mode-map (kbd "G") 'end-of-buffer)
(define-key view-mode-map (kbd "v") 'View-scroll-page-backward)
(define-key view-mode-map (kbd "J") 'scroll-up-line)
(define-key view-mode-map (kbd "K") 'scroll-down-line)
(define-key view-mode-map (kbd "h") 'backward-char)
(define-key view-mode-map (kbd "j") 'next-line)
(define-key view-mode-map (kbd "k") 'previous-line)
(define-key view-mode-map (kbd "l") 'forward-char)
(define-key view-mode-map (kbd "o") 'other-window)
(define-key view-mode-map (kbd "a") 'move-beginning-of-line)
(define-key view-mode-map (kbd "e") 'move-end-of-line)
(define-key view-mode-map (kbd "w") 'kill-ring-save)
(define-key view-mode-map (kbd "f") 'forward-word)
(define-key view-mode-map (kbd "b") 'backward-word)
(define-key view-mode-map (kbd "E") 'my-View-exit)
(define-key view-mode-map (kbd "q") 'bury-buffer)

  (defun my-View-exit ()
  "If readonly file, use sudo to open it."
  (interactive)
  (let ((file buffer-file-name))
    (if (file-writable-p file) (View-exit)
(progn
  (kill-buffer (current-buffer))
      (setq file (concat "/sudo::" file))
    (find-file file)))
))



;; man mode
(add-hook 'Man-mode-hook #'(lambda () (view-mode)))

;; help mode
(add-hook 'help-mode-hook #'(lambda () (view-mode)))


(defcustom doc-view-ghostscript-options
  '("-dNOPAUSE" "-sDEVICE=png256" "-dTextAlphaBits=1"
	"-dBATCH" "-dGraphicsAlphaBits=1" "-dQUIET"
	"-r100")
  "A list of options to give to ghostview."
  :type '(sexp)
  :group 'doc-view)

(setq doc-view-resolution 300)
(setq doc-view-cache-directory "~/.docview")

(defun doc-view-save-attribute (tag value)
  (when (boundp 'doc-view-already-continued-p)
  (unless (bmkp-get-autofile-bookmark buffer-file-name)
	  (bmkp-autofile-set buffer-file-name))
  (bmkp-set-tag-value (eshell/basename buffer-file-name) tag value)))

(defadvice doc-view-set-slice (after doc-view-store-slice activate)
  (doc-view-save-attribute "slice" (doc-view-current-slice)))

(defadvice doc-view-reset-slice (after doc-view-store-slice activate)
  (doc-view-save-attribute "slice" nil))

(defadvice doc-view-goto-page (after doc-view-set-current-page (page) activate)
  (doc-view-save-attribute "page" page))

(defadvice doc-view-enlarge (after doc-view-set-image-width (factor) activate)
  (doc-view-save-attribute "width" doc-view-image-width))

(defadvice doc-view-shrink (after doc-view-set-image-width (factor) activate)
  (doc-view-save-attribute "width" doc-view-image-width))

(defadvice doc-view-scale-reset (after doc-view-set-image-width () activate)
  (doc-view-save-attribute "width" doc-view-image-width))

(defun doc-view-continue-reading ()
  (when (and (eq major-mode 'doc-view-mode) (get-buffer-window (current-buffer)) (not (boundp 'doc-view-already-continued-p)))
	(when (bmkp-get-autofile-bookmark buffer-file-name)
	  (let* (
			 (bookmark (eshell/basename buffer-file-name))
			 (page (bmkp-get-tag-value bookmark "page"))
			 (width (bmkp-get-tag-value bookmark "width"))
			 (slice (bmkp-get-tag-value bookmark "slice")))
		(if width (setq-local doc-view-image-width width))
		(if page (doc-view-goto-page page))
		(if slice (doc-view-set-slice (nth 0 slice) (nth 1 slice) (nth 2 slice) (nth 3 slice)))))
	(setq-local doc-view-already-continued-p t))
  )

(defun doc-view-fix-stuck-image ()
  (when (and (eq major-mode 'doc-view-mode) (get-buffer-window (current-buffer)) (not (boundp 'buffer-already-displayed-p)))
	(doc-view-toggle-display)
	(doc-view-toggle-display)
	(setq-local buffer-already-displayed-p t)))

(add-hook 'window-configuration-change-hook 'doc-view-fix-stuck-image)
(add-hook 'window-configuration-change-hook #'(lambda () (run-with-timer 0.1 nil 'doc-view-continue-reading)))

(defun doc-view-toggle-modeline ()
  (interactive)
  (if mode-line-format
	  (progn
		(setq-local doc-view-saved-mode-line mode-line-format)
		(setq mode-line-format nil))
	(setq mode-line-format doc-view-saved-mode-line)))

(define-key doc-view-mode-map (kbd "g") 'doc-view-first-page)
(define-key doc-view-mode-map (kbd "G") 'doc-view-last-page)
(define-key doc-view-mode-map (kbd "v") 'doc-view-scroll-down-or-previous-page)
(define-key doc-view-mode-map (kbd "C-c g") 'doc-view-goto-page)
(define-key doc-view-mode-map (kbd "m") 'doc-view-toggle-modeline)

(provide 'init-view)
