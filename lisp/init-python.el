;; Now company-jedi seems to work only under python2
(use-package company-jedi
  :ensure t
  :after python
  :config
  (add-to-list 'company-backends 'company-jedi))

(use-package python
  :defer t
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (company-mode 1)
              (yas-minor-mode-on))))

(provide 'init-python)
