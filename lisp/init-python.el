(require-packages '(company-jedi))

(add-hook 'python-mode-hook #'(lambda () (company-mode 1)))

;; Now company-jedi seems to work only under python2
(require 'company-jedi)
(add-to-list 'company-backends 'company-jedi)

(provide 'init-python)
