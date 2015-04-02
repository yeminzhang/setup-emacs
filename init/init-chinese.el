(package-install-when-not-exist '(chinese-pyim pinyin-search))
(require 'chinese-pyim)

(setq default-input-method "chinese-pyim"
	  pyim-dicts  '((:name "bigdict" :file (concat emacs-configuration-root-dir "pyim/pyim-bigdict.txt") :coding utf-8-unix))
	  pyim-use-tooltip t)

(define-key pyim-mode-map (kbd ".")   'pyim-next-page)
(define-key pyim-mode-map (kbd ",")   'pyim-previous-page)
(global-set-key (kbd "C-S-SPC") 'toggle-input-method)

(global-set-key (kbd "C-S-s") 'isearch-forward-pinyin)
(global-set-key (kbd "C-S-r") 'isearch-backward-pinyin)

(provide 'init-chinese)
