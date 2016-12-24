;; eshell

(after-load 'em-hist
  (setq eshell-history-size 100000))

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

(defun eshell-cd-history ()
  (let ((len (ring-length eshell-last-dir-ring))
        (index 0))
    (if (= len 0)
        (error "Directory ring empty"))
    (eshell-init-print-buffer)
    (while (< index len)
      (eshell-buffered-print
       (concat (ring-ref eshell-last-dir-ring index) "\n"))
      (setq index (1+ index)))
    (eshell-flush)
    ))

(after-load 'helm-eshell
  (defvar my-helm-source-eshell-history
    `((name . "Eshell history")
      (init . (lambda ()
                (let (eshell-hist-ignoredups)
                  ;; Write the content's of ring to file.
                  (eshell-write-history eshell-history-file-name)
                  (with-current-buffer (helm-candidate-buffer 'global)
                    (insert-file-contents eshell-history-file-name)))
                ;; Same comment as in `helm-source-esh'
                (remove-hook 'minibuffer-setup-hook 'eshell-mode)))
      (candidates-in-buffer)
      (keymap . ,helm-eshell-history-map)
      (filtered-candidate-transformer . (lambda (candidates sources)
                                          (reverse candidates)))
      (candidate-number-limit . 9999)
      (action . (lambda (candidate)
                  (progn
                    (eshell)
                    (end-of-buffer)
                    (eshell-kill-input)
                    (insert candidate)
                    (eshell-send-input)))))
    "Helm source for Eshell history."))

(defun my-eshell-execute-history ()
  (interactive)
  (require 'em-hist)
  (require 'helm-eshell)
  (helm
   :prompt "Go to: "
   :candidate-number-limit 25    ;; up to 25 of each
   :sources
   '(
     my-helm-source-eshell-history
     )))

(global-set-key (kbd "<f2>") 'my-eshell-execute-history)

(defun eshell-set-keybindings ()
  (define-key eshell-mode-map (kbd "C-r")
    (lambda ()
      (interactive)
      (let (
            (helm-split-window-default-side 'below))
        (recenter)
        (helm-eshell-history)))))

(add-hook 'eshell-mode-hook 'eshell-set-keybindings)
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

(global-set-key (kbd "C-c e") 'eshell-here)

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
