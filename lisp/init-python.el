(require-packages '(company-jedi))

(after-load 'python
  ;; Now company-jedi seems to work only under python2
  (define-key python-mode-map (kbd "M-.") 'jedi:goto-definition)
  (define-key python-mode-map (kbd "M-,") 'jedi:goto-definition-pop-marker)
  (require 'company-jedi)
  ;; configure semantic used by helm-code-select
  (semantic-mode 1)
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1))

(after-load 'company
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook
          (lambda ()
            (company-mode 1)
            (yas-minor-mode-on)
            (local-set-key (kbd "C-j") 'helm-code-select)
            (local-set-key (kbd "C-c <") 'helm-gtags-previous-history)
            (local-set-key (kbd "C-c >") 'helm-gtags-next-history)
            ))

(provide 'init-python)
