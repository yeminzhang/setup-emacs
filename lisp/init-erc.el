;; ERC
(require 'erc)
(define-key erc-mode-map (kbd "<home>") 'beginning-of-buffer)
(define-key erc-mode-map (kbd "<return>") 'erc-send-and-track-switch)
(define-key erc-mode-map (kbd "<C-return>") 'erc-send-current-line)
(defun erc-connect ()
  "Connect to IM networks using bitlbee."
  (interactive)
  (erc :server "localhost" :port 6667 :nick "myuser"))

;; ERC auto away
(require 'erc-autoaway)
(erc-autoaway-mode 1)
(setq erc-auto-discard-away t)
(setq erc-auto-set-away t)
(setq erc-autoaway-idle-seconds 300)
(setq erc-autoaway-message "Away")
(setq erc-auto-query 'bury)
(setq erc-track-exclude-server-buffer t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                    "324" "329" "332" "333" "353" "477" "305" "306"))
(setq erc-track-showcount t)

(setq erc-insert-timestamp-function 'erc-insert-timestamp-left)
(setq erc-query-display 'buffer)

(add-hook 'window-configuration-change-hook
'(lambda ()
(setq erc-fill-column (window-width))))

(require 'erc-track)

(defadvice erc-track-find-face (around erc-track-find-face-promote-query activate)
  (if (erc-query-buffer-p)
      (setq ad-return-value (intern "erc-current-nick-face"))
    ad-do-it))

(defun erc-send-and-track-switch ()
"Send msg in query buffer and call track."
(interactive)
(if (erc-query-buffer-p) (progn (erc-send-current-line) (erc-track-switch-buffer 1)) (erc-send-current-line)))

;;(add-to-list 'erc-server-PART-functions 'erc-abc)
;;(add-to-list 'erc-server-305-functions 'erc-autoaway-set)

;;(defun erc-abc (a b)
;;"Send msg in query buffer and call track."
;;(message format("%s: %s" a b)))

;;(defun erc-autoaway-set (a b)
;;"Send msg in query buffer and call track."
;;(message format("%s: %s" a b)))

(require 'erc-notify)
(erc-notify-mode 1)

(setq erc-notify-interval 30)
(setq erc-notify-list (list "tudouli" "mama" "hennyjuly" "jiabinwang"))

(defun erc-echo-sign-on (server nick)
(message (format "%s: %s has signed on" server nick)))

(add-hook 'erc-notify-signon-hook 'erc-echo-sign-on)

(defun erc-echo-sign-off (server nick)
(message (format "%s: %s has signed off" server nick)))

(add-hook 'erc-notify-signoff-hook 'erc-echo-sign-off)

;;(add-to-list 'load-path "~/.emacs.d/erc-5.3-extras/")
;;(require 'erc-nicklist)
;;(require 'erc-list-old)

(global-set-key (kbd "<f3>")
  (lambda() (interactive)
(if erc-modified-channels-alist
(erc-track-switch-buffer 1)
    (switch-to-buffer "&bitlbee"))))


(provide 'init-erc)
