(require-packages '(eyebrowse))

;; desktop save
(setq desktop-path (list user-emacs-directory))
(desktop-save-mode 1)
(setq desktop-restore-eager t
      desktop-restore-frames t)
;;(setq desktop-files-not-to-save "^$")

(eyebrowse-mode t)

(setq eyebrowse-wrap-around t)

(add-to-list 'window-persistent-parameters '(window-side . writable))
(add-to-list 'window-persistent-parameters '(window-slot . writable))

(defun session-create (session-name)
  (interactive
   (list (read-from-minibuffer "session name: ")))
  (eyebrowse-create-window-config)
  (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) session-name))

(defun session-kill ()
  (interactive)
  (eyebrowse-close-window-config))

(defun session-switch ()
  (interactive)
  (call-interactively 'eyebrowse-switch-to-window-config))

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

(provide 'init-session)
