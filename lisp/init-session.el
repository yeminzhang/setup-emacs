;; desktop save
(use-package desktop
  :defer t
  :config
  (setq desktop-path (list user-emacs-directory)
        desktop-restore-eager t
        desktop-restore-frames t
        desktop-auto-save-timeout 300)
  )

(desktop-save-mode 1)

(use-package eyebrowse
  :ensure t
  :defer t
  :config
  (setq eyebrowse-wrap-around t
        eyebrowse-mode-line-separator ","
        eyebrowse-new-workspace t)

  (defun session-kill ()
    (interactive)
    (eyebrowse-close-window-config))

  (defun session-switch (session)
    (interactive (list (eyebrowse--read-slot)))
    (if (numberp session) (eyebrowse-switch-to-window-config session)
      (progn
        (eyebrowse-create-window-config)
        (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) session)
        )
      ))

  (defun session-rename (ARG)
    (interactive "P")
    (call-interactively 'eyebrowse-rename-window-config))

  (defun session-next()
    (interactive)
    (call-interactively 'eyebrowse-next-window-config))

  (defun session-previous()
    (interactive)
    (call-interactively 'eyebrowse-prev-window-config))

  (defun session-last()
    (interactive)
    (eyebrowse-last-window-config))

  (defun eyebrowse-format-slot (window-config)
    (let* ((slot (car window-config))
           (tag (nth 2 window-config))
           (format-string (if (and tag (> (length tag) 0))
                              "%t"
                            eyebrowse-slot-format))
           ;; NOTE: `format-spec' sets `deactivate-mark' to t which
           ;; makes `eyebrowse-format-slot' usage in
           ;; `eyebrowse-mode-line-indicator' always deactivate the mark
           ;; after activating it as this triggers mode line updates...
           deactivate-mark)
      (format-spec format-string
                   (format-spec-make ?s slot ?t tag))))

  (defun eyebrowse--read-slot ()
    "Read in a window config SLOT to switch to.
A formatted list of window configs is presented as candidates."
    (let* ((candidates (--map (cons (eyebrowse-format-slot it)
                                    (car it))
                              (eyebrowse--get 'window-configs)))
           (candidate-tag (ido-completing-read
                           "Session: " (mapcar 'car candidates) nil nil))
           (candidate (assoc candidate-tag candidates)))
      (if candidate
          (cdr candidate)
        candidate-tag)))
  :init
  (if (eq default-theme 'zenburn)
      (defface eyebrowse-mode-line-active
        '((t (:foreground "#F0DFAF")))
        "The face used to highlight the current perspective on the modeline."))

  :bind (
  ("C-c s s" . session-switch)
  ("C-c s k" . session-kill)
  ("C-c s r" . session-rename)
  ("C-c s n" . session-next)
  ("C-c s p" . session-previous)
  ("C-c s f" . toggle-frame-fullscreen)
  ("C-c s <tab>" . session-last)
  ("<M-tab>" . session-last)
  ("M-;" . session-switch)
  ("M-o" . session-next)
  ("M-n" . session-next)
  ("M-p" . session-previous)
))

(eyebrowse-mode t)
(provide 'init-session)
