(add-to-list 'load-path (expand-file-name "mu/mu4e" user-emacs-directory))
(require-packages '(mu4e-maildirs-extension))

(setq message-send-mail-function 'smtpmail-send-it
      message-kill-buffer-on-exit t
      smtpmail-queue-mail nil)

(after-load 'mu4e
  (unless email-configured (email-configure))
  (setq mu4e-sent-folder "/Sent"
        mu4e-drafts-folder "/Drafts"
        mu4e-trash-folder "/Deleted"
        mu4e-mu-binary (executable-find "mu")
        mu4e-html2text-command (executable-find "html2text")
        mu4e-update-interval 60
        mu4e-view-show-images t
        mu4e-msg2pdf (executable-find "msg2pdf")
        mu4e-get-mail-command "offlineimap"
        mu4e-sent-messages-behavior 'sent
        mu4e-split-view nil
        mu4e-user-mail-address-list (list user-mail-address)
        mu4e-compose-reply-to-address user-mail-address
        mu4e-compose-signature 'get-message-signature
        mu4e-hide-index-messages t
        mu4e-headers-date-format "%y-%m-%d"
        mu4e-headers-time-format "%H:%M"
        mu4e-bookmarks '(
                         ("g:unread AND NOT m:/Git AND NOT m:/Deleted*" "Unread messages" 117)
                         ("date:today..now AND NOT m:/Git AND NOT m:/Deleted*" "Today's messages" 116)
                         ("date:7d..now AND NOT m:/Git AND NOT m:/Deleted*" "Last 7 days" 119)
                         ("mime:image/*" "Messages with images" 112))

        mu4e-headers-fields '((:human-date . 12)
                              (:maildir . 10)
                              (:from-or-to . 22)
                              (:subject)))
  (define-key mu4e-main-mode-map (kbd "q") 'bury-buffer)
  (define-key mu4e-main-mode-map (kbd "Q") 'mu4e-quit)
  (define-key mu4e-headers-mode-map (kbd "g") 'beginning-of-buffer)
  (define-key mu4e-headers-mode-map (kbd "G") 'end-of-buffer)
  (define-key mu4e-headers-mode-map (kbd "v") 'scroll-down-command)
  (define-key mu4e-headers-mode-map (kbd "h") 'mu4e-headers-query-prev)
  (define-key mu4e-headers-mode-map (kbd "l") 'mu4e-headers-query-next)
  (define-key mu4e-headers-mode-map (kbd "o") 'mu4e-headers-view-message)
  (define-key mu4e-headers-mode-map (kbd "r") 'mu4e-compose-reply)
  (define-key mu4e-headers-mode-map (kbd "R") 'mu4e-headers-mark-for-refile)
  (define-key mu4e-headers-mode-map (kbd "f") 'mu4e-compose-forward)
  (define-key mu4e-headers-mode-map (kbd "q")  'bury-buffer)
  (define-key mu4e-view-mode-map (kbd "v") 'scroll-down-command)
  (define-key mu4e-view-mode-map (kbd "f") 'mu4e-view-go-to-url)
  (define-key mu4e-view-mode-map (kbd "g") 'beginning-of-buffer)
  (define-key mu4e-view-mode-map (kbd "G") 'end-of-buffer)
  (define-key mu4e-view-mode-map (kbd "r") 'mu4e-compose-reply)
  (define-key mu4e-view-mode-map (kbd "f") 'mu4e-compose-forward)
  (define-key mu4e-view-mode-map (kbd "F") 'mu4e-view-go-to-url)
  (define-key mu4e-view-mode-map (kbd "R") 'mu4e-view-mark-for-refile)
  (define-key mu4e-view-mode-map (kbd "q")    (lambda() (interactive)
                                                (mu4e~view-quit-buffer)(switch-to-buffer mu4e~headers-buffer-name)))

  (add-hook 'mu4e-index-updated-hook 'my-mu4e-headers-update)
  )

(defun get-message-signature()
  (concat "BR, " (nth 0 (split-string user-full-name " "))))

(defun email-configure()
  (interactive)
  (let
      (
       (maildir (read-directory-name "Mail Dir: " "~/" mu4e-maildir))
       (smtp-server (read-from-minibuffer "smtp mail server: "))
       (email-addr (read-from-minibuffer "User Email Address: "))
       (full-name (read-from-minibuffer "User Full Name: "))
       )
    (customize-save-variable 'mu4e-maildir maildir)
    (customize-save-variable 'smtpmail-smtp-server smtp-server)
    (customize-save-variable 'user-mail-address email-addr)
    (customize-save-variable 'user-full-name full-name)
    (customize-save-variable 'email-configured t)
    (load custom-file)
    ))

(defun my-mu4e-headers-update()
  (let (
        (maxnum (unless mu4e-headers-full-search mu4e-headers-results-limit))
        )
    (with-current-buffer mu4e~headers-buffer-name
      (mu4e~proc-find
       mu4e~headers-last-query
       mu4e-headers-show-threads
       mu4e-headers-sort-field
       mu4e-headers-sort-direction
       maxnum
       mu4e-headers-skip-duplicates
       mu4e-headers-include-related))))

(defun my-mu4e-open(ARG)
  (interactive "P")
  (require 'mu4e)
  (if (or ARG (not (get-buffer mu4e~headers-buffer-name)))
      (mu4e)
    (switch-to-buffer mu4e~headers-buffer-name)))

(mu4e-maildirs-extension)

(global-set-key (kbd "<f3>") 'my-mu4e-open)

(provide 'init-mu4e)
