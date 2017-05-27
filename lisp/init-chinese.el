(setq default-input-method "chinese-pyim")

(use-package chinese-pyim
  :ensure t
  :defer t)

(global-set-key (kbd "C-S-SPC")
                (lambda ()
                  (interactive)
                  (require 'chinese-pyim)
                  (toggle-input-method)))

(use-package chinese-pyim-core
  :config
;;  (setq pyim-dicts  '((:name "bigdict" :file "~/.emacs.d/pyim/pyim-bigdict.txt" :coding utf-8-unix)))
  (setq pyim-use-tooltip t)
  :bind (:map pyim-mode-map
              ("." . pyim-page-next-page)
              ("," . pyim-page-previous-page)))

(use-package chinese-pyim-greatdict
  :ensure t
  :after chinese-pyim
  :config
  (chinese-pyim-greatdict-enable))

(use-package pinyin-search
  :ensure t
  :bind (
  ("C-S-s" . isearch-forward-pinyin)
  ("C-S-r" . isearch-backward-pinyin)
))

(provide 'init-chinese)
