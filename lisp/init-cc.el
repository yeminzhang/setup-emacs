;; init-cc is for the common part of c-mode, c++-mode, java-mode, etc
(require-packages '(clang-format))

(after-load 'clang-format
  (setq clang-format-executable "clang-format")
  (setq-default clang-format-style "{BasedOnStyle: llvm, IndentWidth: 4}"))

(defun cc-format-buffer ()
  (require 'clang-format)
  (clang-format-buffer))

(after-load 'cc-mode
  (add-hook 'c-mode-common-hook
            (lambda ()
              (company-mode 1)
              (helm-gtags-mode 1)
              (yas-minor-mode-on)
              (local-set-key (kbd "C-i") 'clang-format))))

(provide 'init-cc)
