(use-package helm-proc
:ensure t
:bind ("<f1>" . helm-proc))

(use-package restart-emacs
:ensure t)

(defun print-time ()
  (interactive)
  (message (current-time-string)))

(defun file-type (filename)
  (interactive (list (ido-read-file-name "file: ")))
  (shell-command (concat "file " filename)))

(use-package calculator
  :ensure t
  :config
  (customize-set-variable 'calculator-number-digits 5))

(provide 'init-misc)
