(use-package erlang
  :ensure t
  :defer t
  :config
  (add-hook 'erlang-mode-hook
            (lambda ()
              (company-mode 1)
              (helm-gtags-mode))))

(provide 'init-erlang)
