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

  (defadvice switch-to-buffer (after eyebrowse-add-buffers)
    (let ((buf (ad-get-arg 0)))
      (when buf
        (eyebrowse-add-buffer buf eyebrowse-force-add-buffer))))

  (defadvice display-buffer (after eyebrowse-add-buffers)
    (when ad-return-value
      (let ((buf (ad-get-arg 0))
            (frame (window-frame ad-return-value)))
        (when (and buf frame)
          (with-selected-frame frame
            (eyebrowse-add-buffer buf eyebrowse-force-add-buffer))))))

  (defadvice set-window-buffer (after eyebrowse-add-buffers)
    (let ((buf (ad-get-arg 1))
          (frame (window-frame (ad-get-arg 0))))
      (when (and buf frame)
        (with-selected-frame frame
          (eyebrowse-add-buffer buf eyebrowse-force-add-buffer)))))

  (defun eyebrowse-activate-buffer-management ()
    (ad-activate 'switch-to-buffer)
    (ad-activate 'display-buffer)
    (ad-activate 'set-window-buffer))

  (defun eyebrowse-buffer-in-other-session-p (buffer-name)
    (let ((result nil))
      (dolist (slot (hash-table-keys eyebrowse-buffers))
        (unless (= slot (eyebrowse--get 'current-slot))
          (dolist (buffer (gethash slot eyebrowse-buffers))
            (when (string= buffer-name buffer)
              (setq result t)))
          )
        )
      result))

  (defun eyebrowse-add-buffer (buffer &optional FORCE)
    (let (
          (buffer-list (gethash (eyebrowse--get 'current-slot) eyebrowse-buffers))
          (buffer-str (if (stringp buffer) buffer (buffer-name buffer)))
          )
      (when (or (string= buffer-str "*scratch*")
                (not (eyebrowse-buffer-in-other-session-p buffer-str))
                FORCE)
        (if buffer-list
            (puthash (eyebrowse--get 'current-slot) (delete-dups (cons buffer-str buffer-list)) eyebrowse-buffers)
          (puthash (eyebrowse--get 'current-slot) (list buffer-str) eyebrowse-buffers))
        (add-hook 'kill-buffer-hook 'eyebrowse-remove-buffer)
        )))

  (defun eyebrowse-remove-buffer ()
    (let (
          (buffer-str (if (stringp (current-buffer)) (current-buffer) (buffer-name (current-buffer))))
          )
      (dolist (slot (hash-table-keys eyebrowse-buffers))
        (puthash slot (remove buffer-str (gethash slot eyebrowse-buffers)) eyebrowse-buffers))))

  (defun eyebrowse-set-ido-buffers ()
    "Restrict the ido buffer to the current perspective."
    (let* ((buffer-names (gethash (eyebrowse--get 'current-slot) eyebrowse-buffers))
          (indices (make-hash-table :test 'equal)))
      (cl-loop for elt in ido-temp-list
               for i upfrom 0
               do (puthash elt i indices))
      (setq ido-temp-list
            (sort (-intersection buffer-names ido-temp-list)
                  (lambda (a b)
                    (< (gethash a indices)
                       (gethash b indices)))))))

  (add-hook 'ido-make-buffer-list-hook 'eyebrowse-set-ido-buffers)

  (defun eyebrowse-save-buffers ()
    (with-temp-file eyebrowse-buffers-filename
      (prin1 eyebrowse-buffers (current-buffer))))

  (defun eyebrowse-load-buffers ()
    (if (file-exists-p eyebrowse-buffers-filename)
    (with-temp-buffer
      (insert-file-contents eyebrowse-buffers-filename)
      (goto-char (point-min))
      (setq eyebrowse-buffers
            (read (current-buffer)))))
    )

  (add-hook 'eyebrowse-mode-hook 'eyebrowse-load-buffers)
  (add-hook 'desktop-save-hook 'eyebrowse-save-buffers)
  (add-hook 'desktop-delay-hook 'eyebrowse-activate-buffer-management)
  (add-hook 'desktop-no-desktop-file-hook 'eyebrowse-activate-buffer-management)
  (add-hook 'desktop-not-loaded-hook 'eyebrowse-activate-buffer-management)

  ;; use helm-buffers-list to show all buffers and add buffer to session by force
  (defadvice helm-buffers-list (before eyebrowse-disable-modify-list activate)
    (remove-hook 'ido-make-buffer-list-hook 'eyebrowse-set-ido-buffers)
    (setq eyebrowse-force-add-buffer t)
    )

  (defadvice helm-buffers-list (after eyebrowse-disable-modify-list activate)
    (add-hook 'ido-make-buffer-list-hook 'eyebrowse-set-ido-buffers)
    (setq eyebrowse-force-add-buffer nil))

  :init
  (defvar eyebrowse-buffers (make-hash-table :test 'equal))
  (defvar eyebrowse-force-add-buffer nil)
  (defvar eyebrowse-buffers-filename (expand-file-name "eyebrowse-buffers" user-emacs-directory))

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
