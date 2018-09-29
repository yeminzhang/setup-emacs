;; tramp
(require 'tramp)
(setq password-cache t)
(setq password-cache-expiry nil)
(setq tramp-default-method "ssh")
(setq tramp-default-user nil)
(setq tramp-use-ssh-controlmaster-options nil)
(setq tramp-connection-timeout 5)

(setq auth-sources (list "~/.authinfo"))

(defun ssh-host-list ()
  (let (
        (sconfig-list (tramp-parse-sconfig "~/.ssh/config"))
        (ssh-host-list ()))
    (dolist (login sconfig-list)
      (if (and login (nth 1 login)) (add-to-list 'ssh-host-list (nth 1 login) t)))
    ssh-host-list))

(defun ssh-host-get-password (host)
  (let ((info (nth 0 (auth-source-search
                      :host host))))
    (if info
        (let ((secret (plist-get info :secret)))
          (if (functionp secret)
              (funcall secret)
            secret))
      "")))

(defun ssh-host (host)
  (interactive (list (ido-completing-read "ssh to: " (ssh-host-list))))
  (let* (
         (default-directory user-emacs-directory)
         (multi-term-program (expand-file-name "utils/myssh" user-emacs-directory))
         (multi-term-program-switches host)
         (index 1)
         )
    (multi-term)
;;    (term-send-raw-string "export TERM=xterm")
    ;;  (term-send-return)
    ;; Compute index.
    (while (buffer-live-p (get-buffer (format "*%s<%s>*" host index)))
      (setq index (1+ index)))
    (rename-buffer (format "*%s<%s>*" host index))
    ;; add the renamed buffer name to eyebrowse-buffers
    (switch-to-buffer (current-buffer))
    (with-current-buffer (format "*%s<%s>*" host index)
      (setq term-ansi-at-host host)
      (current-buffer))
    ))

(defun dropbox-start ()
  (interactive)
  (if (executable-find "dropbox")
      (apply 'call-process "dropbox" nil 0 nil
             (list "start"))))

(dropbox-start)

(provide 'init-remote)
