;; init-cc is for the common part of c-mode, c++-mode, java-mode, etc

(use-package clang-format
  :ensure t
  :defer t
  :config
  (setq clang-format-executable "clang-format")
  (setq-default clang-format-style "{BasedOnStyle: llvm, IndentWidth: 4}"))

(use-package cc-mode
  :defer t
  :config
  (defun cc-format-buffer ()
    (clang-format-buffer))
  (add-hook 'c-mode-common-hook
            (lambda ()
              (company-mode 1)
              (helm-gtags-mode 1)
              (yas-minor-mode-on))))

(provide 'init-cc)
