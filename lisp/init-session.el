;; desktop save
(use-package desktop
  :defer t
  :config
  (setq desktop-path (list user-emacs-directory)
        desktop-restore-eager t
        desktop-restore-frames nil
        desktop-auto-save-timeout 300)
  (add-hook 'desktop-after-read-hook 'perspectives-restore-state)
  (add-hook 'desktop-no-desktop-file-hook 'persp-mode)
  (add-hook 'desktop-not-loaded-hook 'persp-mode)

  (defun perspectives-restore-state ()
    (unless (bound-and-true-p persp-mode)
      (persp-mode t))
    ;; get the serialized state off of the frame
    (if (boundp 'perspectives-hash-serialized)
        (progn
          (message "Found state, attempting restore")
          (dolist (elem perspectives-hash-serialized)
            ;; recreate the perspective
            (with-perspective (car elem)
              (dolist (buffer-name (nth 1 (nth 0 (nth 1 elem))))
                ;; add the buffer back to the perspective
                (persp-add-buffer buffer-name)
                )
              (window-state-put (cdr (nth 1 (nth 1 elem))))
              ))
          (when (bound-and-true-p persp-last-name)
            (persp-switch persp-last-name))
          (when (bound-and-true-p persp-curr-name)
            (persp-switch persp-curr-name))
          )
      (message "No state found"))))

(desktop-save-mode 1)

(use-package seq
  :ensure t
  :defer t)

(use-package perspective
  :ensure t
  :defer t
  :config
  (defun perspectives-buffer-name-p (buffer)
    (if (and buffer
             (buffer-name buffer))
        ;;           (not (string-prefix-p "*" (buffer-name buffer)))
        ;;           (not (string-suffix-p "*" (buffer-name buffer))))
        t
      nil))

  (defun perspectives-hash-filter ()
    (require 'seq-24)
    (require 'subr-x)
    (let (
          (result ())
          (keys (hash-table-keys perspectives-hash))
          (curr-persp (if persp-curr (persp-name persp-curr) nil))
          (last-persp (if persp-last (persp-name persp-last) nil)))
      ;; for every perspective...
      (dolist (key keys)
        (with-perspective key
          (let ((persp (gethash key perspectives-hash))
                (wconf (window-state-get (frame-root-window (selected-frame)) 'writable))
                (value ()))
            ;; that isn't killed...
            (when (not (persp-killed persp))
              (add-to-list 'value (cons "buffers"
                                        (list
                                         (mapcar 'buffer-name (seq-filter 'perspectives-buffer-name-p (persp-buffers persp))))))
              (add-to-list 'value (cons "windows" wconf) t)
              (add-to-list 'result (cons key (list value))))))
        )
      ;; return a different variable name so perspectives doesn't clobber it
      (when (> (length result) 0)
        (setq perspectives-hash-serialized result
              persp-curr-name curr-persp
              persp-last-name last-persp))
      ))

  (defun session-kill ()
    (interactive)
    (call-interactively 'persp-kill))

  (defun session-switch ()
    (interactive)
    (call-interactively 'persp-switch))

  (defun session-rename ()
    (interactive)
    (call-interactively 'persp-rename))

  (defun session-next()
    (interactive)
    (call-interactively 'persp-next))

  (defun session-previous()
    (interactive)
    (call-interactively 'persp-prev))

  (defun session-add-buffer ()
    (interactive)
    (call-interactively 'persp-add-buffer))

  (defun session-last()
    (interactive)
    (persp-switch-last))

  (use-package desktop
    :defer t
    :config
    (add-to-list 'desktop-globals-to-save 'perspectives-hash-serialized)
    (add-to-list 'desktop-globals-to-save 'persp-curr-name)
    (add-to-list 'desktop-globals-to-save 'persp-last-name)
    (add-hook 'desktop-save-hook 'perspectives-hash-filter)))

(provide 'init-session)
