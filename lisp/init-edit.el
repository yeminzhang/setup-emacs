(show-paren-mode t)

;; font style and size can be customized and saved in custom.el
(customize-save-default 'default-frame-font "Dejavu Sans Mono:pixelsize=14")
(add-to-list 'default-frame-alist (cons 'font default-frame-font))
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

(set-face-background 'hl-line "#4f4f4f")

;; Tabsn
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

;; whitespace
(setq-default show-trailing-whitespace nil)
(use-package whitespace
  :defer t
  :config
  (setq whitespace-style '(tab-mark)))  ;;turns on white space mode only for tabs
(add-to-list 'write-file-hooks 'delete-trailing-whitespace)

(setq inhibit-field-text-motion t)
(use-package misc
  :bind (("M-f" . forward-to-word)))

;; xclip
(setq x-select-enable-clipboard t)

;; smooth scrolling
(setq scroll-conservatively 101)

;; show number of matches in current search
(use-package anzu
  :ensure t
  :defer t
  :diminish anzu-mode)
(global-anzu-mode t)

(use-package desktop
  :defer t
  :config
  (add-to-list 'desktop-globals-to-save 'kill-ring))

(use-package simple
  :config
  (defun smart-kill-ring-save (&optional arg)
    "When called interactively with no active region, copy the whole line."
    (interactive "^p")
    (if mark-active
        (if (= (region-beginning) (region-end))
            ;; if mark is active but no text is selected, then copy the whole word
            (progn
              (deactivate-mark)
              (call-interactively 'forward-to-word)
              (call-interactively 'backward-word)
              (call-interactively 'mark-sexp)
              (smart-kill-ring-save))
          ;; copy selected region
          (kill-ring-save (region-beginning) (region-end)))
      ;; copy the whole line
      (kill-ring-save (line-beginning-position) (line-beginning-position (+ arg 1)))))

  (defun smart-kill-region (&optional arg)
    "When called interactively with no active region, kill the whole line."
    (interactive "^p")
    (if mark-active (kill-region (region-beginning) (region-end))
      (kill-region (line-beginning-position) (line-beginning-position (+ arg 1)))))
  :bind (
         ("C-w" . smart-kill-region)
         ("M-w" . smart-kill-ring-save)
         ("M-j" . delete-indentation))
  )

(defadvice insert-for-yank (before newline-if-is-linestr (str) activate)
  (when (char-equal (aref str (- (length str) 1)) ?\n)
    (end-of-line)
    (if (= (point) (point-max))
        ;; if it reaches end of buffer, then create a newline
        (newline)
      ;; otherwise we just yank line before beginning of next line
      (call-interactively 'forward-char))))

;; auto indent when paste something
(defadvice insert-for-yank (after indent-region (str) activate)
   (and (not current-prefix-arg)
        (derived-mode-p 'prog-mode)
        (let ((mark-even-if-inactive transient-mark-mode))
          (indent-region (region-beginning) (region-end) nil)
          ;; if it is line copy, then stay in the end of last line
          (when (char-equal (aref str (- (length str) 1)) ?\n)
            (call-interactively 'backward-char))
          )))

;; undo-tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config
;; override default volatile-highlights face
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(vhl/default-face ((t (:inherit secondary-selection :background "DarkKhaki" :foreground "black"))))))
;; volatile-highlights
(volatile-highlights-mode t)

(use-package avy
  :ensure t
  :config
  (setq avy-background nil
        avy-keys '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z))
  :bind (("C-j" . avy-goto-word-1)
         ("M-g M-g" . avy-goto-line)
         ("M-g g" . avy-goto-line))
  )

(use-package iedit
  :ensure t)

(use-package rainbow-mode
  :defer t
  :ensure t)

(rainbow-mode t)

(set-language-environment "UTF-8")

(provide 'init-edit)
