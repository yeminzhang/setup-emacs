(setq emacs-configuration-root-dir "~/.emacs.d/")
(add-to-list 'load-path emacs-configuration-root-dir)
(load (concat emacs-configuration-root-dir "env.el"))
(require 'project)
(add-to-list 'load-path (concat emacs-configuration-root-dir "init"))
(require 'init-helm)
(require 'init-edit)
(require 'init-buffer)
(require 'init-view)
(require 'init-term)
(require 'init-eshell)
(require 'init-dired)
(require 'init-elisp)
(require 'init-c)
(if (and (boundp 'erlang-root-dir) (file-exists-p erlang-root-dir))
(require 'init-erlang))
;;(require 'init-erc)
;;(require 'init-wanderlust)
;;(require 'init-mu4e)
;;(require 'init-ibus)
(require 'init-magit)
(require 'init-company)
(require 'init-appearance)


;;package
;;(require 'package)
;;(add-to-list 'package-archives
  ;;           '("melpa" . "http://melpa.milkbox.net/packages/") t)



;;(global-set-key (kbd "<f1>") 'my-w3m-goto-url-new-session)
;;(global-set-key (kbd "<f6>") 'self-insert-command)


;; xclip
;;(xclip-mode 1)

;;(global-set-key (kbd "å") 'eshell-display-buffer)

;; helm


;; environment variables

;; lotc specific, will move to other file later on
;;(setenv "gw" "/gw:/root")
;;(setenv "build126" "/eyemzha@build126:/home/eyemzha")
;;(setenv "n1dell4" "/n1dell4:/root")
;;(setenv "n1dell1" "/n1dell1:/root")


(defun connect-host (host-id)
(interactive (list (ido-completing-read "Node is: " 
;;(list "dmx1" "dmx2" "hub")
(progn
(setq local-machine-list machine-list)
(setq id-list '())
(while local-machine-list
  (setq id-list (append id-list (list (plist-get (pop local-machine-list) :id))))
)
id-list
)
)))
(if (eq (get-buffer host-id) nil)
(progn 
(setq local-machine-list machine-list)
(setq id "")
(setq password "")
(while local-machine-list
(setq machine (pop local-machine-list))
(if (eq (plist-get machine :id) host-id)
(progn
(setq password (plist-get machine :password))
)
))
(eshell-exec-visual "sshpass" "-p" password "ssh" host-id)
(set-buffer (get-buffer "*sshpass*"))
(rename-buffer host-id))
(if (eq (get-buffer-process host-id) nil) (progn (kill-buffer host-id) (nodeconnect host-id)) (switch-to-buffer host-id))))

(defun labtelnet (hostname)
(interactive (list (ido-completing-read "Node is: " (list "n1dell1" "n1dell2"))))
(setq line (shell-command-to-string (concat "grep " hostname " ~/utils/lab_hosts")))
(setq console_ip (nth 4 (split-string line)))
(setq console_port (nth 5 (split-string line)))
(eshell-exec-visual "telnet" console_ip console_port)
(set-buffer (get-buffer "*telnet*"))
(rename-buffer (concat hostname "-telnet")))


;; eshell auto completion

;;(defconst pcmpl-cd-history '("ab" "cd") "List of cd history")

;;(defun pcomplete/cd ()
 ;; "Completion for cd"
 ;; (pcomplete-here pcmpl-cd-history)
 ;; (cond (pcomplete-match "xx" 2)
;;	  (while (pcomplete-here "/"))))

;; chinese input
;;(set-language-environment 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-clipboard-coding-system 'euc-cn)
;; (set-clipboard-coding-system 'utf-8)
;; (set-clipboard-coding-system 'cn-gb-2312)
;; (set-terminal-coding-system 'utf-8)
;; (set-buffer-file-coding-system 'utf-8)
;; (set-buffer-file-coding-system 'cn-gb-2312)
;; (set-selection-coding-system 'euc-cn)
;; (set-selection-coding-system 'chinese-iso-8bit-with-esc)
;; (set-selection-coding-system 'cn-gb-2312)
;; (set-default-coding-systems 'utf-8)
 ;;(set-default-coding-systems 'cn-gb-2312)
;; (setq locale-coding-system 'cp1252)
;; (modify-coding-system-alist 'process "*" 'utf-8)
;; (setq default-process-coding-system '(utf-8 . utf-8))
;; (setq-default pathname-coding-system 'utf-8)


;;(eshell-display-buffer)

;; weibo
;;(add-to-list 'load-path "/home/eyemzha/.emacs.d/weibo")
;;(require 'weibo)
;add-to-list(add-to-list 'load-path "~/.emacs.d/google-maps/")
;(require 'google-maps)

;; todo
;;(unless splitted (progn (split-window-right) (setq splitted t)))

(add-to-list 'load-path (concat emacs-configuration-root-dir "workgroups2/src"))
(require 'workgroups2)
(setq wg-mode-line-disable t)
(workgroups-mode 1)

(add-hook 'emacs-startup-hook #'(lambda () (wg-reload-session)))

