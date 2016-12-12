(require-packages '(company))

(after-load 'company
  (setq company-idle-delay 0)
  (define-key company-active-map (kbd right-little-finger-key) 'company-complete-selection)
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort))

(provide 'init-company)
