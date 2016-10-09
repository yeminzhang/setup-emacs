(require-packages '(multi-term))

(require 'term)
(require 'multi-term)

(defun term-send-f12    () (interactive) (term-send-raw-string "\033\[24~"))
(global-set-key (kbd "<f12>") 'term-send-f12)

;; term mode
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq ansi-color-for-comint-mode t)

(setq term-default-bg-color nil)
(setq term-default-fg-color nil)

(define-key term-raw-map (kbd "C-o") 'other-window)
(define-key term-raw-map (kbd "<prior>") 'scroll-down-command)
(define-key term-raw-map (kbd "<next>") 'scroll-up-command)
;;(define-key term-raw-map (kbd "C-h k") 'describe-key)
(define-key term-raw-map (kbd "<home>") 'beginning-of-buffer)
(define-key term-raw-map (kbd "<end>") 'end-of-buffer)
(define-key term-raw-map (kbd "M-x") 'smex)

;; key map
(define-key function-key-map "\e[24~" [f5])

(delete '("M-o" . term-send-backspace) term-bind-key-alist)

(defun term-switch-to-terminal-frame ()
  (let (
        (frame (or (get-a-frame "terminal") (make-frame '((name . "terminal"))))))
    (select-frame-set-input-focus frame)))

(defun bash-term ()
  (interactive)
  (let ((multi-term-program "/bin/bash"))
    (term-switch-to-terminal-frame)
    (multi-term)))

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
