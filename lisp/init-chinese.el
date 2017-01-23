(setq default-input-method "chinese-pyim")

(use-package chinese-pyim
  :ensure t
  :defer t)

(use-package chinese-pyim-core
  :defer t
  :config
;;  (setq pyim-dicts  '((:name "bigdict" :file "~/.emacs.d/pyim/pyim-bigdict.txt" :coding utf-8-unix)))
  (setq pyim-use-tooltip t))

(use-package chinese-pyim-greatdict
  :ensure t
  :after chinese-pyim
  :config
  (chinese-pyim-greatdict-enable))

(use-package pinyin-search
  :ensure t
  :defer t)

(provide 'init-chinese)
