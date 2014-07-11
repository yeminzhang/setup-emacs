;; eshell
(require 'eshell)
(if (eq (get-buffer eshell-buffer-name) nil) (eshell))
(add-to-list 'eshell-visual-commands "vim")
(add-to-list 'eshell-visual-commands "git log")
(add-to-list 'eshell-visual-commands "telnet")
(add-to-list 'eshell-visual-commands "ssh")
(add-to-list 'eshell-visual-commands "sshpass")


(add-hook 'eshell-post-command-hook 'eshell-modify-cmd-history)
(setq eshell-history-size 512)

(defun eshell-modify-cmd-history ()
 (progn (setq my-last-ring (ring-ref eshell-history-ring 0))
	   (ring-remove eshell-history-ring 0)
	   (setq my-last-ring (chomp my-last-ring))
	   (setq modified-ring my-last-ring)
	   (unless (or (string= (substring my-last-ring 0 1) "{") (string= (substring my-last-ring 0 1) "("))
	   (setq modified-ring (concat "{" (abbreviate-file-name (eshell/pwd)) ";  " my-last-ring "} ")))
;;	   (unless (or (ring-member eshell-history-ring modified-ring) (string= (substring last-ring 0 2) "cd") (< (length last-ring) 8))
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

(require 'windmove)

(defun eshell-display-buffer ()
(interactive)
(progn (unless (or (one-window-p) (null (windmove-find-other-window 'left)))
(windmove-left))
(if (and (string= (buffer-name) eshell-buffer-name) (not (one-window-p)))
(windmove-right))
(delete-other-windows)
(split-window-horizontally)
(other-window 1)
(switch-to-buffer eshell-buffer-name)
))

(require 'helm-eshell)

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
;;		(switch-to-buffer eshell-buffer-name))))
		(end-of-buffer))))
  ;;              (eshell-kill-input)
    ;;            (insert candidate)
;;		(eshell-send-input))))
  "Helm source for Eshell history.")

(defun my-eshell-execute-history ()
(interactive)
;;(unless (eq eshell-mode t)
;;(progn
;;(other-window 1)
;;(switch-to-buffer eshell-buffer-name)))
(helm
   :prompt "Go to: "
   :candidate-number-limit 25                 ;; up to 25 of each
   :sources
   '(
	 my-helm-source-eshell-history
 )))

(global-set-key (kbd "<f2>") 'my-eshell-execute-history)

(add-hook 'eshell-mode-hook #'
  (lambda()
	(local-set-key (kbd "C-r") 'my-eshell-execute-history)))

(provide 'init-eshell)
