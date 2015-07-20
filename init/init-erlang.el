(require-packages '(erlang))
(require 'erlang-start)

;;(add-to-list 'load-path "/home/eyemzha/.emacs.d/distel/elisp")
;;(require 'distel)
;;(distel-setup)

(setq distel-tags-compliant nil)

(add-hook 'erlang-mode-hook #'(lambda () (company-mode 1)))

(provide 'init-erlang)
