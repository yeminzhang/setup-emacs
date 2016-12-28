(require-packages '(wconf))

;; desktop save
(setq desktop-path (list user-emacs-directory))
(desktop-save-mode 1)
(setq desktop-restore-eager t
      desktop-restore-frames nil)
;;(setq desktop-files-not-to-save "^$")

(require 'wconf)

(add-hook 'desktop-after-read-hook 'session-start)
(add-hook 'desktop-not-loaded-hook 'session-start)
(add-hook 'desktop-no-desktop-file-hook 'session-start)

(after-load 'desktop
  (add-to-list 'desktop-globals-to-save 'wconf--index)
  (add-to-list 'desktop-globals-to-save 'session-name-list)
  (add-to-list 'desktop-globals-to-save 'session-index-last-used))

(defun session-start ()
  (interactive)
  (let (
        (index (if (boundp 'wconf--index) wconf--index nil))
        )
    (unless (boundp 'session-name-list) (setq session-name-list (list)))
    (if index
        (progn
          (wconf-load)
          (wconf-switch-to-config index))
      (session-create "default"))))

(defun session-create (session-name)
  (interactive
   (list (read-from-minibuffer "session name: ")))
  (setq session-index-last-used wconf--index)
  (add-to-list 'session-name-list session-name t)
  (wconf-create t))

(defun session-kill ()
  (interactive)
  (delete (nth wconf--index session-name-list) session-name-list)
  (setq session-index-last-used nil)
  (wconf-kill))

(defun session-switch-by-index (index)
  (when (/= index wconf--index)
    (setq session-index-last-used wconf--index)
    (wconf-switch-to-config index)))

(defun session-switch (session-name)
  (interactive
   (list (ido-completing-read "session name: " session-name-list)))
  (let (
        (index (position session-name session-name-list))
        )
    (session-switch-by-index index)))

(defun session-switch-to-last-used ()
  (interactive)
  (when (and (boundp 'session-index-last-used) session-index-last-used)
    (session-switch-by-index session-index-last-used)))

(defun session-save ()
  (interactive)
  (wconf-store-all)
  (wconf-save))

(add-hook 'kill-emacs-hook 'session-save)

(provide 'init-session)
