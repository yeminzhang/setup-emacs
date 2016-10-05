;; tramp
(require 'tramp)
(setq password-cache t)
(setq password-cache-expiry nil)
(setq tramp-default-method "ssh")
(setq tramp-default-user nil)
(setq recentf-auto-cleanup 'never)
(setq tramp-use-ssh-controlmaster-options nil)
(setq tramp-connection-timeout 5)

(setq auth-sources (list "~/.authinfo"))

(defcustom ssh-tunnel-autorun-list '()
  "A list of hosts which will be established as ssh tunnels automatically."
  :type 'list)

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

(defun ssh-tunnel-configure-autorun()
  (interactive)
  (let (
        (sconfig-list (tramp-parse-sconfig "~/.ssh/config"))
        (ssh-tunnel-list ()))
    (dolist (login sconfig-list)
      (if (and login (nth 1 login) (y-or-n-p (concat "Add " (nth 1 login) " to ssh tunnel list? ")))
          (add-to-list 'ssh-tunnel-list (nth 1 login) t)))
    (customize-save-variable 'ssh-tunnel-autorun-list ssh-tunnel-list)))

(defun ssh-tunnel-run-all-autorun ()
  (interactive)
  (if (boundp 'ssh-tunnel-autorun-list)
      (dolist (host ssh-tunnel-autorun-list) (ssh-tunnel-run host))))

(defun ssh-tunnel-kill-all ()
  (interactive)
  (dolist (host (ssh-host-list)) (ssh-tunnel-kill host)))

(defun ssh-tunnel-run (host)
  (interactive (list (ido-completing-read "setup tunnel: " (ssh-host-list))))
  (unless (ssh-tunnel-running-p host) (ssh-tunnel-command host :run)))

(defun ssh-tunnel-check (host)
  (interactive (list (ido-completing-read "check tunnel: " (ssh-host-list))))
  (if (eql 0 (ssh-tunnel-command host :check)) (message "running") (message "not running")))

(defun ssh-tunnel-kill (host)
  (interactive (list (ido-completing-read "kill tunnel: " (ssh-host-list))))
  (if (ssh-tunnel-running-p host) (ssh-tunnel-command host :kill)))

(defun ssh-tunnel-running-p (host)
  (eql 0 (ssh-tunnel-command host :check)))

(defun ssh-tunnel-command (host command)
  (let* (
         (args (cond ((eq command :run)
                      (list "-M" "-f" "-N" "-T"))
                     ((eq command :kill)
                      (list "-O" "exit"))
                     ((eq command :check)
                      (list "-O" "check"))
                     (t (error "Unknown ssh-tunnels command '%s'" command))))
         (destination (cond ((eq command :check) nil)
                            ((eq command :run) 0)
                            ((eq command :kill) 0)
                            (t nil))))
    (apply 'call-process "ssh" nil destination nil
           (append args
                   (list host)))))

(defun ssh-host (host)
  (interactive (list (ido-completing-read "ssh to: " (ssh-host-list))))
  (let (
        (default-directory user-emacs-directory)
        (password (ssh-host-get-password host))
        )
    (if (not (string= password ""))
        (progn
          (eshell-exec-visual "sshpass" "-p" password "ssh" host)
          (set-buffer (get-buffer "*sshpass*")))
      (progn
        (eshell-exec-visual "ssh" host)
        (set-buffer (get-buffer "*ssh*"))))
    (rename-buffer (concat "ssh-" host))))

(defun ssh-tunnel-start-timer ()
  (interactive)
  (unless (boundp 'ssh-tunnel-monitor-timer)
    (setq ssh-tunnel-monitor-timer (run-with-timer 60 60 'ssh-tunnel-run-all-autorun))))

(defun ssh-tunnel-stop-timer ()
  (interactive)
  (unless (not (boundp 'ssh-tunnel-monitor-timer))
    (cancel-timer ssh-tunnel-monitor-timer)
    (makunbound 'ssh-tunnel-monitor-timer)))

(defun dropbox-start ()
  (interactive)
  (if (executable-find "dropbox")
      (apply 'call-process "dropbox" nil 0 nil
             (list "start"))))

(autoload 'eshell-exec-visual "em-term")
(ssh-tunnel-run-all-autorun)
(ssh-tunnel-start-timer)
(dropbox-start)

(provide 'init-remote)
