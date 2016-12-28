;; eshell

(after-load 'em-hist
  (setq eshell-history-size 100000
        eshell-hist-ignoredups t))

(after-load 'em-dirs
  (setq eshell-last-dir-ring-size 10000))

(defun eshell-notify-done ()
  (if (string-prefix-p "scp" (ring-ref eshell-history-ring 0))
      (message (concat (ring-ref eshell-history-ring 0) " done!"))))

;; make sure eshell buffer is on focus
(defun eshell-run-command (command)
  (end-of-buffer)
  (eshell-kill-input)
  (insert command)
  (eshell-send-input))

(defun eshell-cmd-valuable-p (command)
  (cond
   ((string= command "x") nil)
   ((string= command "pwd") nil)
   ((s-starts-with-p "cd" command) nil)
   ((file-directory-p command) nil)
   (t)))

(defun eshell-persistent-command (command)
  (let ((first-char (substring command 0 1)))
    (if (or (string= first-char "{") (string= first-char "("))
        command
      (concat "{cd " (abbreviate-file-name (eshell/pwd)) ";  " command "} "))))

(defun eshell-modify-cmd-history ()
  (let* (
         (last-command (chomp (ring-ref eshell-history-ring 0))))
    (ring-remove eshell-history-ring 0)
    (when (eshell-cmd-valuable-p last-command)
      (let* (
             (last-command (eshell-persistent-command last-command))
             (index (ring-member eshell-history-ring last-command)))
        (when index (ring-remove eshell-history-ring index))
        (ring-insert eshell-history-ring last-command)
        (eshell-write-history)))))

(defun helm-eshell-dir-history ()
  "Preconfigured helm for eshell history."
  (interactive)
  (require 'helm-elisp)
  (require 'helm-eshell)
  (let* ((end   (point))
         (beg   (save-excursion (eshell-bol) (point)))
         (input (buffer-substring beg end))
         flag-empty)
    (when (eq beg end)
      (insert " ")
      (setq flag-empty t)
      (setq end (point)))
    (unwind-protect
        (with-helm-show-completion beg end
          (helm :sources (helm-make-source "Eshell dir history"
                             'helm-eshell-dir-history-source)
                :buffer "*helm eshell dir history*"
                :resume 'noresume
                :input input))
      (when (and flag-empty
                 (looking-back " " (1- (point))))
        (delete-char -1)))))

(defclass helm-eshell-dir-history-source (helm-source-sync)
  ((init :initform
         (lambda ()
           ;; Same comment as in `helm-source-esh'.
           (remove-hook 'minibuffer-setup-hook 'eshell-mode)))
   (candidates
    :initform
    (lambda ()
      (with-helm-current-buffer
        (cl-loop for c from 0 to (ring-length eshell-last-dir-ring)
                 collect (ring-ref eshell-last-dir-ring c)))))
   (nomark :initform t)
   (multiline :initform t)
   (keymap :initform helm-eshell-history-map)
   (candidate-number-limit :initform 9999)
   (action :initform (lambda (candidate)
                       (cd candidate)
                       (eshell-run-command ""))))
  "Helm class to define source for Eshell dir history.")

(add-hook 'eshell-mode-hook 'eshell-register-desktop-save)
(add-hook 'eshell-mode-hook 'company-mode)

(after-load 'em-term
  (add-to-list 'eshell-visual-commands "vim")
  (add-to-list 'eshell-visual-commands "git log")
  (add-to-list 'eshell-visual-commands "telnet")
  (add-to-list 'eshell-visual-commands "ssh")
  (add-to-list 'eshell-visual-commands "sshpass")
  (add-to-list 'eshell-visual-commands "tclsh8.5"))

(add-hook 'eshell-post-command-hook 'eshell-modify-cmd-history)
(add-hook 'eshell-post-command-hook 'eshell-notify-done)

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffers file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                  default-directory)))
    (maybe-split-window t)
    (eshell)
    (cd parent)
    (eshell-run-command "")))

(defun eshell/x ()
  (bury-buffer))

;; save eshell buffer when save desktop
(defun eshell-register-desktop-save ()
  "Set `desktop-save-buffer' to a function returning nothing."
  (setq desktop-save-buffer (lambda (desktop-dirname) default-directory)))

(defun eshell-restore-desktop-buffer (d-b-file-name d-b-name d-b-misc)
  "Restore a `eshell' buffer on `desktop' load."
  (let (
      (default-directory d-b-misc))
    (eshell)
    (get-buffer eshell-buffer-name)
    ))

(after-load 'desktop
  (add-to-list 'desktop-buffer-mode-handlers '(eshell-mode . eshell-restore-desktop-buffer)))

;; eshell auto completion

;;(defconst pcmpl-cd-history '("ab" "cd") "List of cd history")

;;(defun pcomplete/cd ()
;; "Completion for cd"
;; (pcomplete-here pcmpl-cd-history)
;; (cond (pcomplete-match "xx" 2)
;;	  (while (pcomplete-here "/"))))

(provide 'init-eshell)
