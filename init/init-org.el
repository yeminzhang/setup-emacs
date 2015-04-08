(require 'org)
(require 'org-agenda)
(setq org-completion-use-ido t
	  org-log-done t)

(unless (file-exists-p org-directory) (mkdir org-directory))
(unless org-agenda-files (setq org-agenda-files (list (concat org-directory "/life.org") (concat org-directory "/work.org"))))

(dolist (agenda-file org-agenda-files)
  (unless (file-exists-p agenda-file) (shell-command (concat "touch " agenda-file))))

(defun org-agenda-bury ()
  (interactive)
  (bury-buffer)
  (other-window 1))

(define-key org-agenda-mode-map (kbd "q") 'org-agenda-bury)

(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)


(setq org-todo-keywords
	  (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
			  (sequence "WAITING(w@/!)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persistence-insinuate t)
(setq org-clock-persist t)
(setq org-clock-in-resume t)
;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")
;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Show clock sums as hours and minutes, not "n days" etc.
(setq org-time-clocksum-format
	  '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))
(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))
(add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)
;;(after-load 'org-clock
;;			(define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
;;			(define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))


;; restore org-agenda when emacs starts
;; save org-agenda buffer when save desktop
(defun org-agenda-register-desktop-save ()
  "Set `desktop-save-buffer' to a function returning the type of current agenda."
  (setq desktop-save-buffer (lambda (desktop-dirname) org-agenda-type)))

(add-hook 'org-agenda-mode-hook 'org-agenda-register-desktop-save)

(defun org-agenda-restore-desktop-buffer (d-b-file-name d-b-name d-b-misc)
  "Restore a `org-agenda' buffer on `desktop' load."
  (when (eq 'org-agenda-mode desktop-buffer-major-mode)
		(org-agenda-list)
        (current-buffer)))

(add-to-list 'desktop-buffer-mode-handlers '(org-agenda-mode . org-agenda-restore-desktop-buffer))

(provide 'init-org)
