(require-packages '(chinese-pyim pinyin-search))

(setq default-input-method "chinese-pyim")

(after-load 'chinese-pyim-core
  (setq pyim-dicts  '((:name "bigdict" :file "~/.emacs.d/pyim/pyim-bigdict.txt" :coding utf-8-unix)))
  (setq pyim-use-tooltip t)
  (define-key pyim-mode-map (kbd ".") 'pyim-next-page)
  (define-key pyim-mode-map (kbd ",") 'pyim-previous-page))

(global-set-key (kbd "C-S-SPC")
                (lambda ()
                  (interactive)
                  (require 'chinese-pyim)
                  (toggle-input-method)))

(global-set-key (kbd "C-S-s") 'isearch-forward-pinyin)
(global-set-key (kbd "C-S-r") 'isearch-backward-pinyin)

(provide 'init-chinese)
