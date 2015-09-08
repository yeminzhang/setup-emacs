;; init-cc is for the common part of c-mode, c++-mode, java-mode, etc
(require-packages '(clang-format))

(after-load 'clang-format
  (setq clang-format-executable "clang-format")
  (setq-default clang-format-style "{BasedOnStyle: llvm, IndentWidth: 4}"))

(defun cc-format-buffer ()
  (require 'clang-format)
  (clang-format-buffer))

(after-load 'cc-mode
  ;; configure semantic
  (semantic-mode 1)
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  ;; ede for semantic
  (global-ede-mode)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (company-mode 1)
              (helm-gtags-mode 1)
              (local-set-key (kbd "C-i") 'clang-format))))

(provide 'init-cc)
