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

(defun term-copy ()
  (interactive)
;;  (let* ((mark-command-begin (progn (term-send-raw-string "\C-a") (point)))
;;    (mark-command-end (progn (end-of-line) (point))))
;;    (if mark-active (kill-ring-save (region-beginning) (region-end))
  ;;    (kill-ring-save mark-command-begin mark-command-end))))
  (let* ((proc (get-buffer-process (current-buffer)))
         (pmark (process-mark proc))
         (pmark-val (marker-position pmark))
         (input-is-new (>= (point) pmark-val))
         (intxt (if input-is-new
                    (progn (if term-eol-on-send (end-of-line))
                           (buffer-substring pmark (point)))
                  (funcall term-get-old-input))))
    (message intxt)))

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

(defun term-here ()
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory)))
    (maybe-split-window t)
    (let (
          (default-directory parent))
    (multi-term))))

(add-hook 'term-mode-hook 'term-register-desktop-save)

;; save multi-term buffer when save desktop
(defun term-register-desktop-save ()
  "Set `desktop-save-buffer' to a function returning nothing."
  (setq desktop-save-buffer (lambda (desktop-dirname) (concat term-ansi-at-host " " default-directory))))

(defun term-restore-desktop-buffer (d-b-file-name d-b-name d-b-misc)
  "Restore a `multi-term' buffer on `desktop' load."
  (let (
        (addr-pair (split-string d-b-misc " ")))
        (if (string= (nth 0 addr-pair) (system-name))
            ;;localhost
            (let ((default-directory (nth 1 addr-pair)))
              (multi-term)
              (rename-buffer d-b-name)
              (current-buffer))
          ;; ssh-host
          (ssh-host (nth 0 addr-pair))
    )))

(after-load 'desktop
  (add-to-list 'desktop-buffer-mode-handlers '(term-mode . term-restore-desktop-buffer)))


(provide 'init-term)
