(require-packages '(erlang))

(add-hook 'erlang-mode-hook
          (lambda ()
            (company-mode 1)
            (helm-gtags-mode)))

(provide 'init-erlang)
