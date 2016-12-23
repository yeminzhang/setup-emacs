;; eshell

(after-load 'em-hist
  (setq eshell-history-size 512))

(defun eshell-notify-done ()
  (if (string-prefix-p "scp" (ring-ref eshell-history-ring 0))
      (message (concat (ring-ref eshell-history-ring 0) " done!"))))

(defun eshell-modify-cmd-history ()
  (progn (setq my-last-ring (ring-ref eshell-history-ring 0))
         (ring-remove eshell-history-ring 0)
         (setq my-last-ring (chomp my-last-ring))
         (setq modified-ring my-last-ring)
         (unless (or (string= (substring my-last-ring 0 1) "{") (string= (substring my-last-ring 0 1) "("))
           (setq modified-ring (concat "{" (abbreviate-file-name (eshell/pwd)) ";  " my-last-ring "} ")))
         (setq index (ring-member eshell-history-ring modified-ring))
         (if index (ring-remove eshell-history-ring index))
         (ring-insert eshell-history-ring modified-ring)
         (eshell-write-history)))

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

(add-hook 'eshell-mode-hook
          (lambda()
            (local-set-key (kbd "C-r")
                           (lambda()
                             (interactive)
                             (let (
                                   (helm-split-window-default-side 'below))
                               (helm-eshell-history))))
            (add-to-list 'eshell-visual-commands "vim")
            (add-to-list 'eshell-visual-commands "git log")
            (add-to-list 'eshell-visual-commands "telnet")
            (add-to-list 'eshell-visual-commands "ssh")
            (add-to-list 'eshell-visual-commands "sshpass")
            (add-to-list 'eshell-visual-commands "tclsh8.5")
            (eshell-register-desktop-save)
            (add-hook 'eshell-post-command-hook 'eshell-modify-cmd-history)
            (add-hook 'eshell-post-command-hook 'eshell-notify-done)
            ))

;;(defun eshell-here ()
;;  (interactive)
;;  (let (
;;        (dir default-directory)
;;        )
;;    (eshell)
;;    (cd dir)
;;    (end-of-buffer)
;;    (eshell-kill-input)
;;    (eshell-send-input)))

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffers file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (setq eshell-window-splitted t)
    (eshell)
    (cd parent)
    (end-of-buffer)
    (eshell-kill-input)
    (eshell-send-input)))

(defun eshell/x ()
  (if (and (boundp 'eshell-window-splitted) eshell-window-splitted)
      (progn
        (delete-window)
        (setq eshell-window-splitted nil))
  (bury-buffer)))

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
