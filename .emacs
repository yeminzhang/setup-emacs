(add-to-list 'load-path "/home/eyemzha/.emacs.d")
(add-to-list 'load-path "/home/eyemzha/.emacs.d/smex")
(add-to-list 'load-path "/home/eyemzha/.emacs.d/helm")
(add-to-list 'load-path "/home/eyemzha/.emacs.d/mu/mu4e")
(add-to-list 'load-path "/home/eyemzha/.emacs.d/magit")
(add-to-list 'load-path "/home/eyemzha/.emacs.d/init")
(require 'init-helm)
(require 'init-edit)
(require 'init-buffer)
(require 'init-view)
(require 'init-term)
(require 'init-eshell)
;;(require 'init-w3m)
(require 'init-elisp)
(require 'init-c)
(require 'init-erlang)
;;(require 'init-erc)
;;(require 'init-wanderlust)
(require 'init-mu4e)
;;(require 'init-ibus)
(require 'init-appearance)


;; dired
(setq dired-listing-switches "-lhaD")
(setq dired-isearch-filenames t)
(setq dired-recursive-deletes 1)
(setq delete-by-moving-to-trash t)
(setq dired-recursive-copies "always")
;; underline the current line in dired mode
(add-hook 'dired-mode-hook #'(lambda () (hl-line-mode 1)))
(add-hook 'dired-mode-hook #'(lambda () (local-set-key (kbd "b") 'scroll-down-command)))
(add-hook 'dired-mode-hook #'(lambda () (local-set-key (kbd " ") 'scroll-up-command)))
(add-hook 'dired-mode-hook #'(lambda () (rename-buffer (concat "d-" (buffer-name)))))
;;(setq auto-revert-interval 3)
;;(add-hook 'dired-mode-hook #'(lambda () (auto-revert-mode 1)))
(setq dired-auto-revert-buffer t)



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



(defun labssh (hostname)
(interactive (list (ido-completing-read "Node is: " (list "dmx1" "dmx2"))))
(if (eq (get-buffer hostname) nil)
(progn (setq line (shell-command-to-string (concat "grep " hostname " ~/utils/lab_hosts")))
(setq ssh_ip (nth 1 (split-string line)))
(setq ssh_username (nth 2 (split-string line)))
(setq ssh_password (nth 3 (split-string line)))
(eshell-exec-visual "sshpass" "-p" ssh_password "ssh" (format "%s@%s" ssh_username ssh_ip))
(set-buffer (get-buffer "*sshpass*"))
(rename-buffer hostname))
(if (eq (get-buffer-process hostname) nil) (progn (kill-buffer hostname) (labssh hostname)) (switch-to-buffer hostname))))

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

