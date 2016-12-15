(require-packages '(multi-term))

(require 'term)
(require 'multi-term)

(defun term-send-f12    () (interactive) (term-send-raw-string "\033\[24~"))
(global-set-key (kbd "<f12>") 'term-send-f12)

;; term mode
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq ansi-color-for-comint-mode t)
(setq term-buffer-maximum-size 0)

(setq term-default-bg-color nil)
(setq term-default-fg-color nil)

(define-key term-raw-map (kbd "C-o") 'other-window)
(define-key term-raw-map (kbd "<prior>") 'scroll-down-command)
(define-key term-raw-map (kbd "<next>") 'scroll-up-command)
;;(define-key term-raw-map (kbd "C-h k") 'describe-key)
(define-key term-raw-map (kbd "<home>") 'beginning-of-buffer)
(define-key term-raw-map (kbd "<end>") 'end-of-buffer)
(define-key term-raw-map (kbd "M-x") 'smex)
(define-key term-raw-map (kbd "C-v") 'scroll-up-command)
;; sometimes we need to use vi in term-mode. so we need to make esc work
(define-key term-raw-map (kbd "<escape>") 'term-send-esc)

;; key map
(define-key function-key-map "\e[24~" [f5])

(delete '("M-o" . term-send-backspace) term-bind-key-alist)

;; bind C-r to search shell command history, and M-r to search buffer
(delete '("M-r" . term-send-reverse-search-history) term-bind-key-alist)
(delete '("C-r" . isearch-backward) term-bind-key-alist)
(add-to-list 'term-bind-key-alist '("M-r" . isearch-backward))
(add-to-list 'term-bind-key-alist '("C-r" . term-send-reverse-search-history))

(defun term-switch-to-terminal-frame ()
  (let (
        (frame (or (get-a-frame "terminal") (make-frame '((name . "terminal"))))))
    (select-frame-set-input-focus frame)))

;;(defadvice multi-term (before multi-term-switch-to-frame activate)
;;  (term-switch-to-terminal-frame))

(defadvice multi-term (before multi-term-set-program activate)
  (when (file-exists-p "/bin/zsh") (setenv "SHELL" "/bin/zsh")))

(defun term-send-function-key ()
  (interactive)
  (let* ((char last-input-event)
         (output (cdr (assoc char term-function-key-alist))))
    (term-send-raw-string output)))

(defconst term-function-key-alist '((f1 . "\e[OP")
                                    (f2 . "\e[OQ")
                                    (f3 . "\e[OR")
                                    (f4 . "\e[OS")
									(f5 . "\e[24~")))

(dolist (spec term-function-key-alist)
  (define-key term-raw-map
    (read-kbd-macro (format "<%s>" (car spec)))
    'term-send-function-key))

(defun term-toggle-submode ()
(interactive)
(if (term-in-char-mode) (term-line-mode) (term-char-mode)))

(define-key term-mode-map (kbd "M-SPC") 'term-toggle-submode)
(define-key term-raw-map (kbd "M-SPC") 'term-toggle-submode)

(provide 'init-term)
