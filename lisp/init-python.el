(require-packages '(company-jedi))

(after-load 'python
  ;; Now company-jedi seems to work only under python2
  (require 'company-jedi))

(after-load 'company
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook
          (lambda ()
            (company-mode 1)
            (yas-minor-mode-on)))

(provide 'init-python)
