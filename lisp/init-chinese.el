(require-packages '(chinese-pyim pinyin-search))

(setq default-input-method "chinese-pyim")

(after-load 'chinese-pyim-core
  (setq pyim-dicts  '((:name "bigdict" :file "~/.emacs.d/pyim/pyim-bigdict.txt" :coding utf-8-unix)))
  (setq pyim-use-tooltip t))

(provide 'init-chinese)
