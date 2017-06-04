(use-package company
  :ensure t
  :defer t
  :config
  (setq company-idle-delay 0)
  :bind (:map company-active-map
              ("C-n" . company-select-next-or-abort)
              ("C-p" . company-select-previous-or-abort)
              (";" . company-complete-selection))
)

(provide 'init-company)
