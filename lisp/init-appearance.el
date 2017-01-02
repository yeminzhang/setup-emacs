(require-packages '(zenburn-theme diminish smart-mode-line rainbow-mode))
;; Appearance
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)
(setq inhibit-startup-message t)
;;(display-time)
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode t)
(setq split-height-threshold nil)

;; font style and size can be customized and saved in custom.el
(unless (boundp 'default-frame-font)
  (customize-save-variable 'default-frame-font "Monospace:pixelsize=14"))
(add-to-list 'default-frame-alist (cons 'font default-frame-font))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;;(set-default-font "opendesktop-fonts")
;;(setq font-use-system-font t)
;;(add-to-list 'default-frame-alist '(font . "Monospace-12"))
;;(add-to-list 'default-frame-alist '(font . "AR PL New Sung-12"))
;;(set-fontset-font t  '(#x00 . #x7)  "Monospace")
;;(set-face-background 'highlight nil)
;;(set-face-foreground 'highlight nil)
;;(set-face-underline-p 'highlight t)
;; (set-background-color "grey5")
;; (set-foreground-color "white")
;; (set-cursor-color "red")

(set-fontset-font t  '(#x80 . #x3FFFFF)  "Microsoft YaHei")

;; Chinese Font, needs to be verified and improved in the future
;;(dolist (charset '(kana han symbol cjk-misc bopomofo))
;;  (set-fontset-font (frame-parameter nil 'font)
;;                  charset
;;                (font-spec :family "Microsoft YaHei" :size 22)))

;; set the status bar color of window
;;(set-face-attribute  'mode-line
;;                 nil
;;                :foreground "black"
;;              :background "grey70"
;;            :box '(:line-width 1 :style released-button))
;;(set-face-attribute  'mode-line-inactive
;;               nil
;;             :foreground "white"
;;           :background "grey20"
;;         :box '(:line-width 1 :style released-button))


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


(global-hl-line-mode 1)
(column-number-mode 1)

(after-load 'ido
  (setq ido-max-window-height 1)
  (setq max-mini-window-height 1))

;; color theme
(load-theme 'zenburn t)

;;(set-face-attribute 'region nil :background "#666" :foreground "#ffffff")

;; override default volatile-highlights face
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(vhl/default-face ((t (:inherit secondary-selection :background "tan" :foreground "black")))))

(after-load 'smart-mode-line
  (setq sml/theme nil
        sml/shorten-modes t
        sml/mode-width 'right
        sml/shorten-directory t
        sml/name-width '(10 . 35)
        sml/show-frame-identification t))
(sml/setup)

;; display batter usage for notebook
(after-load 'battery
  (setq battery-mode-line-format "[%b%p%%] "))
;; display time in 24hr format
(after-load 'time
  (setq display-time-24hr-format t
        display-time-string-forms (delete 'load display-time-string-forms)))

(display-battery-mode 1)
(display-time)

(rainbow-mode t)

(diminish 'volatile-highlights-mode)
(diminish 'projectile-mode)

(provide 'init-appearance)
