(setq default-input-method "pyim")

(use-package pyim
  :ensure t
  :defer t)

(global-set-key (kbd "C-S-SPC")
                (lambda ()
                  (interactive)
                  (require 'pyim)
                  (toggle-input-method)))

(use-package pyim-core
  :config
  (setq pyim-use-tooltip t)
  :bind (:map pyim-mode-map
              ("." . pyim-page-next-page)
              ("," . pyim-page-previous-page)))

(use-package pyim-basedict
  :ensure t
  :after pyim
  :config
  (pyim-basedict-enable))

(use-package pinyin-search
  :ensure t
  :bind (
  ("C-S-s" . isearch-forward-pinyin)
  ("C-S-r" . isearch-backward-pinyin)
))

(provide 'init-chinese)
