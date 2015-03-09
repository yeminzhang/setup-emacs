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

(defadvice doc-view-set-slice (after doc-view-store-slice  activate)
(setq-local doc-view-my-slice (doc-view-current-slice)))

(defadvice doc-view-reset-slice (after doc-view-store-slice  activate)
(kill-local-variable 'doc-view-my-slice))

(defun doc-view-restore-slice ()
(interactive)
(if (boundp 'doc-view-my-slice)
  (doc-view-set-slice (nth 0 doc-view-my-slice) (nth 1 doc-view-my-slice) (nth 2 doc-view-my-slice) (nth 3 doc-view-my-slice))))

(defadvice doc-view-goto-page (after doc-view-set-current-page (page) activate)
(setq-local doc-view-last-page-index page))

(defun doc-view-continue-reading ()
(interactive)
(if (and (eq major-mode 'doc-view-mode) (get-buffer-window (current-buffer)) (not (boundp 'doc-view-already-continued-p)))
(progn
(if (boundp 'doc-view-last-page-index) (doc-view-goto-page doc-view-last-page-index))
(doc-view-restore-slice)
(setq-local doc-view-already-continued-p t)
)))

(define-key doc-view-mode-map (kbd "c") 'doc-view-continue-reading)

(defun doc-view-fix-stuck-image ()
(if (and (eq major-mode 'doc-view-mode) (get-buffer-window (current-buffer)) (not (boundp 'buffer-already-displayed-p)))
(progn
(setq doc-view-tmp-image-width nil)
(setq doc-view-tmp-my-slice nil)
(setq doc-view-tmp-last-page-index nil)
(if (boundp 'doc-view-image-width) (setq doc-view-tmp-image-width doc-view-image-width))
(if (boundp 'doc-view-my-slice) (setq doc-view-tmp-my-slice doc-view-my-slice))
(if (boundp 'doc-view-last-page-index) (setq doc-view-tmp-last-page-index doc-view-last-page-index))
(doc-view-toggle-display)
(doc-view-toggle-display)
(if doc-view-tmp-image-width (setq-local doc-view-image-width doc-view-tmp-image-width))
(if doc-view-tmp-my-slice (setq-local doc-view-my-slice doc-view-tmp-my-slice))
(if doc-view-tmp-last-page-index (setq-local doc-view-last-page-index doc-view-tmp-last-page-index))
(setq-local buffer-already-displayed-p t))))

(add-hook 'window-configuration-change-hook 'doc-view-fix-stuck-image)
(add-hook 'window-configuration-change-hook #'(lambda () (run-with-timer 0.1 nil 'doc-view-continue-reading)))

(require 'desktop)
(add-to-list 'desktop-locals-to-save 'doc-view-image-width)
(add-to-list 'desktop-locals-to-save 'doc-view-my-slice)
(add-to-list 'desktop-locals-to-save 'doc-view-last-page-index)

(provide 'init-view)
