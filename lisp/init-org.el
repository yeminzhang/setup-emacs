(after-load 'org
  (setq org-completion-use-ido t
        org-log-done 'time
        org-log-reschedule 'time
        org-log-redeadline 'time
        org-todo-keywords
        (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                (sequence "WAITING(w@/!)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)")))
        ;; Show clock sums as hours and minutes, not "n days" etc.
        org-time-clocksum-format
        '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))

(defun org-agenda-bury ()
  (interactive)
  (bury-buffer)
  (other-window 1))

(after-load 'org-agenda
  (add-hook 'org-agenda-mode-hook 'org-agenda-register-desktop-save))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(after-load 'org-clock
  (setq org-clock-persistence-insinuate t
        org-clock-persist t
        org-clock-in-resume t
        ;; Change task state to STARTED when clocking in
        org-clock-in-switch-to-state "STARTED"
        ;; Save clock data and notes in the LOGBOOK drawer
        org-clock-into-drawer t
        ;; Removes clocked tasks with 0:00 duration
        org-clock-out-remove-zero-time-clocks t)
  (add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
  (add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
  (add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line))

;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))
(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

;; restore org-agenda when emacs starts
;; save org-agenda buffer when save desktop
(defun org-agenda-register-desktop-save ()
  "Set `desktop-save-buffer' to a function returning the type of current agenda."
  (setq desktop-save-buffer (lambda (desktop-dirname) org-agenda-type)))

(defun org-agenda-restore-desktop-buffer (d-b-file-name d-b-name d-b-misc)
  "Restore a `org-agenda' buffer on `desktop' load."
  (when (eq 'org-agenda-mode desktop-buffer-major-mode)
    (org-agenda-list)
    (current-buffer)))

(after-load 'desktop
  (add-to-list 'desktop-buffer-mode-handlers
               '(org-agenda-mode . org-agenda-restore-desktop-buffer)))

(provide 'init-org)
