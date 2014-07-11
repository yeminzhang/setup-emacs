;; wanderlust
(add-to-list 'load-path "/home/eyemzha/.emacs.d/wanderlust/wl")
(add-to-list 'load-path "/home/eyemzha/.emacs.d/wanderlust/utils")
(add-to-list 'load-path "/home/eyemzha/.emacs.d/wanderlust/elmo")
(require 'wl)
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

(define-key wl-summary-mode-map (kbd "g") 'beginning-of-buffer)
(define-key wl-summary-mode-map (kbd "G") 'end-of-buffer)

(setq wl-queue-folder ".queue")

(setq elmo-imap4-debug t)
;; IMAP
;;(setq elmo-imap4-default-server "mail.internal.ericsson.com")
;;(setq elmo-imap4-default-user "eyemzha")
;;(setq elmo-imap4-default-authenticate-type 'clear)
;;(setq elmo-imap4-default-port '993)
;;(setq elmo-imap4-default-stream-type 'ssl)
;;(setq elmo-imap4-use-modified-utf7 t)


(require 'elmo-search)
(elmo-search-register-engine
    'mu 'local-file
    :prog "/usr/bin/mu" ;; or wherever you've installed it
    :args '("find" pattern "--fields" "l") :charset 'utf-8)

(setq elmo-search-default-engine 'mu)


(setq
  elmo-maildir-folder-path "~/Maildir"          ;; where i store my mail

  wl-smtp-posting-server "localhost" )           ;; put the smtp server here
;;  wl-local-domain "myhost.example.com"          ;; put something here...
;;  wl-message-id-domain "myhost.example.com"     ;; ...

  ;;wl-from "Me <me@example.com>"                  ;; my From:

  ;; note: all below are dirs (Maildirs) under elmo-maildir-folder-path
  ;; the '.'-prefix is for marking them as maildirs
;;  wl-fcc ".Sent"                       ;; sent msgs go to the "sent"-folder
;;  wl-fcc-force-as-read t               ;; mark sent messages as read
;;  wl-default-folder ".Inbox"           ;; my main inbox
;;  wl-draft-folder ".drafts"            ;; store drafts in 'postponed'
;;  wl-trash-folder ".trash"             ;; put trash in 'trash'
;;  wl-spam-folder ".trash"              ;; ...spam as well
;;  wl-queue-folder ".queue")             ;; we don't use this


;; SMTP
;;(setq wl-smtp-connection-type 'starttls)
(setq wl-smtp-posting-port 587)
(setq wl-smtp-authenticate-type "plain")
(setq wl-smtp-posting-user "mattofransen")
(setq wl-smtp-posting-server "smtp.gmail.com")
(setq wl-local-domain "gmail.com")

(setq wl-default-folder ".INBOX")
(setq wl-default-spec ".")
(setq wl-draft-folder ".Drafts")
(setq wl-trash-folder ".Deleted Items")

(setq wl-folder-check-async t)

(setq elmo-imap4-use-modified-utf7 t)

(autoload 'wl-user-agent-compose "wl-draft" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

(setq
  wl-message-ignored-field-list '("^.*:")
  wl-message-visible-field-list
  '("^\\(To\\|Cc\\):"
    "^Subject:"
    "^\\(From\\|Reply-To\\):"
    "^Organization:"
;;    "^Message-Id:"
    "^\\(Posted\\|Date\\):"
    )
  wl-message-sort-field-list
  '(
   "^Subject"
   "^From"
;;    "^Organization:"
  ;;  "^X-Attribution:"
       "^Date"
     "^To"
     "^Cc"))

(setq wl-summary-line-format "%Y-%M-%D (%W) %h:%m %t%[%25(%c %f%) %] %s")
(setq wl-summary-indent-length-limit nil)
(setq wl-summary-width 115)
(setq wl-message-window-size '(5 . 5))
;;(setq wl-biff-check-folder-list '("%Inbox/Only" "%Inbox/To" "%Inbox/Cc" "%Inbox/Team" "%Inbox/Unit"))
(setq wl-biff-check-folder-list '(".INBOX.Only" ".INBOX.To" ".INBOX.Cc" ".INBOX.Team" ".INBOX.Unit"))

;; update summaries when new mail has come
(defun my-wl-update-current-summaries ()
  (let ((buffers (wl-collect-summary)))
    (while buffers
      (with-current-buffer (car buffers)
        (save-excursion
          (wl-summary-sync-update)))
      (setq buffers (cdr buffers)))))

(add-hook
 'wl-biff-notify-hook
 '(lambda ()
    (my-wl-update-current-summaries)
    ))

;;(setq wl-summary-redisplay-hook nil)

(require 'w3m)
(require 'mime-w3m)

;; let sent box show recipient instead of sender
(setq wl-summary-showto-folder-regexp ".*Sent.*")
(setq wl-user-mail-address-list  '("yemin.zhang@ericsson.com"))

(defun wl-summary-default-from (from)
  "Instance of `wl-summary-from-function'.
Ordinarily returns the sender name. Returns recipient names if (1)
summary's folder name matches with `wl-summary-showto-folder-regexp'
and (2) sender address is yours.

See also variable `wl-use-petname'."
  (let ((translator (if wl-use-petname
			(lambda (string)
			  (or (funcall wl-summary-get-petname-function string)
			      (car (std11-extract-address-components string))
			      string))
		      #'identity))
	to ng)
    (or (and (eq major-mode 'wl-summary-mode)
	     (stringp wl-summary-showto-folder-regexp)
	     (string-match wl-summary-showto-folder-regexp
			   (wl-summary-buffer-folder-name))
	     (wl-address-user-mail-address-p from)
	     (cond
	      ((setq to (elmo-message-entity-field wl-message-entity 'to))
	       (mapconcat translator to ","))
	      ((setq ng (elmo-message-entity-field wl-message-entity
						   'newsgroups))
	       (concat "Ng:" ng))))
	(funcall translator from))))

(add-hook 'wl-summary-sync-updated-hook 'my-wl-sort-headers)

(defun my-wl-sort-headers()
(wl-summary-rescan "date" 1))


;; sort the list in summary buffer in rdate order
;;(add-to-list 'wl-summary-sort-specs 'rdate)

;;  (defun wl-summary-overview-entity-compare-by-rdate (x y)
  ;;  (not (wl-summary-overview-entity-compare-by-date x y)))
  ;;(add-to-list 'wl-summary-sort-specs 'rdate)

  ;; Or the infinitesimally more efficient:
  ;; (wl-summary-overview-entity-compare-by-size y x)

  ;;http://osdir.com/ml/mail.wanderlust.general/2004-06/msg00052.html
;;  ;;You could define yourself a sort helper function:
  ;;(defun wl-summary-sort-by-rdate ()
;;    (interactive)
  ;;  (wl-summary-rescan "rdate")
    ;;(goto-char (point-min)))

  ;;and bind that to a key, or (kludgier) define an after advice for
  ;;wl-summary-rescan that moves the cursor if the sort argument begins
  ;;with "r". I haven't tested this, but it would go something like:

;;  (defadvice wl-summary-rescan (after wl-summary-rescan-move-cursor activate)
;;    (if (string-match "^r" (ad-get-arg 0))
  ;;      (goto-char (point-min))))


(defun wl-message-select-buffer (buffer)
  "Select BUFFER as a message buffer."
 (switch-to-buffer-other-window buffer))


(defun wl-folder-jump-to-current-entity (&optional arg)
  "Enter the current folder.  If optional ARG exists, update folder list."
  (interactive "P")
  (let ((fld-name (wl-folder-get-entity-from-buffer))
	entity beg end indent opened err)
    (unless fld-name
      (error "No folder"))
    (beginning-of-line)
    (if (and (wl-folder-buffer-group-p)
	     (looking-at wl-folder-group-regexp))
	;; folder group
	(save-excursion
	  (setq indent (wl-match-buffer 1))
	  (setq opened (wl-match-buffer 2))
	  (if (string= opened "+")
	      (progn
		(setq entity (wl-folder-search-group-entity-by-name
			      fld-name
			      wl-folder-entity))
		(setq beg (point))
		(if arg
		    (wl-folder-update-recursive-current-entity entity)
		  ;; insert as opened
		  (setcdr (assoc (car entity) wl-folder-group-alist) t)
		  (if (eq 'access (cadr entity))
		      (wl-folder-maybe-load-folder-list entity))
		  ;(condition-case errobj
		  (progn
		    (if (or (wl-folder-force-fetch-p (car entity))
			    (and
			     (eq 'access (cadr entity))
			     (null (caddr entity))))
			(wl-folder-update-newest indent entity)
		      (wl-folder-insert-entity indent entity))
		    (wl-highlight-folder-path wl-folder-buffer-cur-path))
;;;		  (quit
;;;		   (setq err t)
;;;		   (setcdr (assoc fld-name wl-folder-group-alist) nil))
;;;		  (error
;;;		   (elmo-display-error errobj t)
;;;		   (ding)
;;;		   (setq err t)
;;;		   (setcdr (assoc fld-name wl-folder-group-alist) nil)))
		  (if (not err)
		      (let ((buffer-read-only nil))
			(delete-region (save-excursion (beginning-of-line)
						       (point))
				       (save-excursion (end-of-line)
						       (+ 1 (point))))))))
	    (setq beg (point))
	    (end-of-line)
	    (save-match-data
	      (setq end
		    (progn (wl-folder-goto-bottom-of-current-folder indent)
			   (beginning-of-line)
			   (point))))
	    (setq entity (wl-folder-search-group-entity-by-name
			  fld-name
			  wl-folder-entity))
	    (let ((buffer-read-only nil))
	      (delete-region beg end))
	    (setcdr (assoc (car entity) wl-folder-group-alist) nil)
	    (wl-folder-insert-entity indent entity) ; insert entity
	    (forward-line -1)
	    (wl-highlight-folder-path wl-folder-buffer-cur-path)
;;;	    (wl-delete-all-overlays)
;;;	    (wl-highlight-folder-current-line)
	    ))
      ;; ordinal folder
      (wl-folder-set-current-entity-id
       (get-text-property (point) 'wl-folder-entity-id))
      (setq fld-name (wl-folder-get-folder-name-by-id
		      wl-folder-buffer-cur-entity-id))
      (let ((summary-buf (wl-summary-get-buffer-create fld-name arg))
	    error-selecting)
	(if (or wl-stay-folder-window wl-summary-use-frame)
	    (wl-folder-select-buffer summary-buf)
;;	  (if (and summary-buf
;;		   (get-buffer-window summary-buf))
;;	      )
)
	(wl-summary-goto-folder-subr fld-name
				     (wl-summary-get-sync-range
				      (wl-folder-get-elmo-folder fld-name))
				     nil t t))))
  (set-buffer-modified-p nil))


(provide 'init-wanderlust)
