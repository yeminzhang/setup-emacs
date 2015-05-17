;; tramp
(require 'tramp)
(setq password-cache t)
(setq password-cache-expiry nil)
(setq tramp-default-method "ssh")
(setq tramp-default-user nil)
(setq recentf-auto-cleanup 'never)
(setq tramp-use-ssh-controlmaster-options nil)
(setq tramp-connection-timeout 5)
(add-to-list 'ido-ignore-buffers "\*tramp")

(defun ssh-host-list ()
  (let (
	(sconfig-list (tramp-parse-sconfig "~/.ssh/config"))
	(ssh-host-list ()))
    (dolist (login sconfig-list)
      (if (and login (nth 1 login)) (add-to-list 'ssh-host-list (nth 1 login) t)))
    ssh-host-list))

(defun ssh-tunnel-run-all-preconfigured ()
  (interactive)
  (if (boundp 'ssh-tunnel-host-list)
  (dolist (host ssh-tunnel-host-list) (ssh-tunnel-run host))))

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
                     (t (error "Unknown ssh-tunnels command '%s'" command)))))
    (apply 'call-process "ssh" nil 0 nil
           (append args
                   (list host)))))

(defun ssh-host (host)
  (interactive (list (ido-completing-read "ssh to: " (ssh-host-list))))
  (eshell-exec-visual "ssh" host)
  (set-buffer (get-buffer "*ssh*"))
  (rename-buffer (concat "ssh-" host)))

(defun ssh-tunnel-start-timer ()
  (interactive)
  (unless (boundp 'ssh-tunnel-monitor-timer)
	(setq ssh-tunnel-monitor-timer (run-with-timer 60 60 'ssh-tunnel-run-all-preconfigured))))

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

(ssh-tunnel-run-all-preconfigured)
(ssh-tunnel-start-timer)
(dropbox-start)

(provide 'init-remote)
