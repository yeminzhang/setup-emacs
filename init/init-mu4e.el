(require 'mu4e)
(require 'mu4e-view)
(setq mu4e-maildir "/home/eyemzha/Maildir") 
(setq mu4e-sent-folder "/Sent Items") 
(setq mu4e-drafts-folder "/Drafts") 
(setq mu4e-trash-folder "/Deleted Items") 
(setq mu4e-mu-binary "/usr/local/bin/mu") 
(setq mu4e-html2text-command "w3m -dump -cols 100 -T text/html")
;;(setq mu4e-html2text-command "/usr/bin/html2text")
(setq mu4e-update-interval 60)
(setq mu4e-view-show-images t)
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-smtp-server "smtp.internal.ericsson.com")
(setq smtpmail-queue-mail nil)

(setq mu4e-sent-messages-behavior 'sent)
(setq mu4e-split-view nil)
(setq mu4e-user-mail-address-list (list "yemin\.zhang@ericsson\.com"))
(setq user-mail-address "yemin.zhang@ericsson.com")
(setq user-full-name "Yemin Zhang")
(setq mu4e-reply-to-address "yemin.zhang@ericsson.com")
(setq message-signature "BR, Yemin")
(setq message-kill-buffer-on-exit t)

(define-key mu4e-headers-mode-map (kbd "g") 'beginning-of-buffer)
(define-key mu4e-headers-mode-map (kbd "G") 'end-of-buffer)
(define-key mu4e-headers-mode-map (kbd "v") 'scroll-down-command)
(define-key mu4e-headers-mode-map (kbd "h") 'mu4e-headers-query-prev)
(define-key mu4e-headers-mode-map (kbd "l") 'mu4e-headers-query-next)
(define-key mu4e-headers-mode-map (kbd "o") 'mu4e-headers-view-message)
(define-key mu4e-headers-mode-map (kbd "r") 'mu4e-compose-reply)
(define-key mu4e-headers-mode-map (kbd "R") 'mu4e-headers-mark-for-refile)
(define-key mu4e-headers-mode-map (kbd "f") 'mu4e-compose-forward)
;;(define-key mu4e-headers-mode-map (kbd "q")  (lambda() (interactive) (switch-to-buffer (other-buffer))))
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

(setq mu4e-bookmarks '(
 ("g:unread AND NOT m:/Git AND NOT m:/Deleted*" "Unread messages" 117)
 ("date:today..now AND NOT m:/Git AND NOT m:/Deleted*" "Today's messages" 116)
 ("date:7d..now AND NOT m:/Git AND NOT m:/Deleted*" "Last 7 days" 119)
 ("mime:image/*" "Messages with images" 112)))

(setq mu4e-headers-fields '((:human-date . 12)
 (:maildir . 10)
 (:from-or-to . 22)
 (:subject)))

(setq mu4e-maildir-shortcuts '(
("/inbox" . ?i)
("/Cc" . ?c)
("/Only" . ?n)
("/To" . ?t)
("/Team" . ?e)
("/Unit" . ?u)
("/Git" . ?g)
("/Deleted Items" . ?d)
("/Sent Items" . ?s)))

(setq mu4e-headers-date-format "%y-%m-%d")
(setq mu4e-headers-time-format "%H:%M")

(add-hook 'mu4e-index-updated-hook 'my-mu4e-headers-update)

(defun my-mu4e-headers-update()
(with-current-buffer mu4e~headers-buffer-name
  (setq maxnum (unless mu4e-headers-full-search mu4e-headers-results-limit))
    (mu4e~proc-find
      mu4e~headers-last-query
      mu4e-headers-show-threads
      mu4e~headers-sort-field
      mu4e~headers-sort-direction
      maxnum
      mu4e-headers-skip-duplicates
      mu4e-headers-include-related)))

(global-set-key (kbd "<f4>")
  (lambda() (interactive)
    (switch-to-buffer mu4e~headers-buffer-name)))

(provide 'init-mu4e)
