(require 'term)

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
;;(define-key term-raw-map (kbd "C-y") 'yank)


(add-hook 'term-mode-hook #'(lambda () (local-set-key (kbd "C-o") 'other-window)))
;; multi-term
;;(add-to-list 'load-path "/home/eyemzha/.emacs.d/")
;;(require 'multi-term)

;; key map
(define-key function-key-map "\e[24~" [f5])

(defun bash-term ()
(interactive)
(ansi-term "/bin/bash"))

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


;; yank in term
(define-key term-raw-map "\C-y"
  (lambda ()
    (interactive)
    (term-send-string (get-process (buffer-name (current-buffer))) (current-kill 0))))

(provide 'init-term)
