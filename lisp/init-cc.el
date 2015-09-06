;; init-cc is for the common part of c-mode, c++-mode, java-mode, etc
(require-packages '(clang-format))
(require 'cc-mode)

(require 'clang-format)
(setq clang-format-executable "clang-format")
(setq-default clang-format-style "{BasedOnStyle: llvm, IndentWidth: 4}")

(defun cc-format-buffer ()
  (clang-format-buffer))

(add-hook 'c-mode-common-hook #'(lambda () (local-set-key (kbd "C-i") 'clang-format)))

(provide 'init-cc)
