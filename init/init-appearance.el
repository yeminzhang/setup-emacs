;; Appearance
 (menu-bar-mode -1)
 (tool-bar-mode -1)
 (scroll-bar-mode -1)
 (blink-cursor-mode 0)
 (setq inhibit-startup-message t)
;;(display-time)
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode t)
(setq max-mini-window-height nil)
;;(set-default-font "Monospace-10:pixelsize=14")
(set-default-font "Monospace:pixelsize=14")
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

(global-hl-line-mode 1)
(column-number-mode 1)

(require 'ido)
(setq ido-max-window-height 1)
(setq max-mini-window-height 1)

;; color theme
(add-to-list 'custom-theme-load-path (concat emacs-configuration-root-dir "zenburn/"))
(load-theme 'zenburn t)

(set-face-attribute 'region nil :background "#666" :foreground "#ffffff")

(split-window-right)

(provide 'init-appearance)
