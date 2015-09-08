(require-packages '(company-jedi))

(after-load 'python
  ;; Now company-jedi seems to work only under python2
  (define-key python-mode-map (kbd "M-.") 'jedi:goto-definition)
  (define-key python-mode-map (kbd "M-,") 'jedi:goto-definition-pop-marker)
  (require 'company-jedi))

(after-load 'company
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook
          (lambda ()
            (company-mode 1)
            (local-set-key (kbd "C-j") 'helm-code-select)
            (local-set-key (kbd "C-c <") 'helm-gtags-previous-history)
            (local-set-key (kbd "C-c >") 'helm-gtags-next-history)
            ))

(provide 'init-python)
