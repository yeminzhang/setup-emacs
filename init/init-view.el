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


(provide 'init-view)
