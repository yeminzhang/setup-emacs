(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(defun customize-save-default (symbol value)
  (unless (boundp symbol)
    (customize-save-variable symbol value)))

(use-package s
  :ensure t)

(provide 'init-utils)
