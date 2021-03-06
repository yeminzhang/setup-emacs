;; view mode
(setq view-read-only t)

(use-package view
  :defer t
  :config
  (defun my-View-exit ()
    "If readonly file, use sudo to open it."
    (interactive)
    (let ((file buffer-file-name))
      (if (file-writable-p file) (View-exit)
        (progn
          (kill-buffer (current-buffer))
          (setq file (concat "/sudo::" file))
          (find-file file)))))
  :bind (:map view-mode-map
              ("G" . end-of-buffer)
              ("v" . View-scroll-page-backward)
              ("J" . scroll-up-line)
              ("K" . scroll-down-line)
              ("h" . backward-char)
              ("j" . next-line)
              ("k" . previous-line)
              ("l" . forward-char)
              ("o" . other-window)
              ("a" . move-beginning-of-line)
              ("e" . move-end-of-line)
              ("w" . kill-ring-save)
              ("f" . forward-word)
              ("b" . backward-word)
              ("E" . my-View-exit)
              ("q" . bury-buffer)))

;; man mode
(use-package man
  :defer t
  :config
  (add-hook 'Man-mode-hook 'view-mode))

;; help mode
(use-package help-mode
  :defer t
  :config
  (add-hook 'help-mode-hook #'(lambda () (view-mode)))
  (set-display-buffer-other-window (rx bos "*Help*" eos)))

;; doc view
(use-package doc-view
  :defer t
  :init
  (defcustom doc-view-ghostscript-options
    '("-dNOPAUSE" "-sDEVICE=png256" "-dTextAlphaBits=1"
      "-dBATCH" "-dGraphicsAlphaBits=1" "-dQUIET"
      "-r100")
    "A list of options to give to ghostview."
    :type '(sexp)
    :group 'doc-view)
  :config
  (setq doc-view-resolution 300)
  (setq doc-view-cache-directory "~/.docview")
  (bookmark-load-if-not)
  (add-hook 'window-configuration-change-hook 'doc-view-fix-stuck-image)
  (add-hook 'window-configuration-change-hook
            #'(lambda () (run-with-timer 0.1 nil 'doc-view-continue-reading)))

  (defun doc-view-save-attribute (tag value)
    (when (boundp 'doc-view-already-continued-p)
      (unless (bmkp-get-autofile-bookmark buffer-file-name)
        (bmkp-autofile-set buffer-file-name))
      (bmkp-set-tag-value (file-name-nondirectory buffer-file-name) tag value)))

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
               (bookmark (file-name-nondirectory buffer-file-name))
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

  (defun doc-view-toggle-modeline ()
    (interactive)
    (if mode-line-format
        (progn
          (setq-local doc-view-saved-mode-line mode-line-format)
          (setq mode-line-format nil))
      (setq mode-line-format doc-view-saved-mode-line)))
  )

(provide 'init-view)
