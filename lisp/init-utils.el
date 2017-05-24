;; disable bold font globally
(defadvice set-face-attribute
    (before ignore-attributes (face frame &rest args) activate)
  (setq args
        (apply 'nconc
               (mapcar (lambda (i)
                         (let ((attribute (nth i args))
                               (value (nth (1+ i) args)))
                           (if (not (memq attribute
                                          set-face-ignore-attributes))
                               (list attribute value))))
                       (number-sequence 0 (1- (length args)) 2)))))

(setq set-face-ignore-attributes '(:weight :height))

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

(use-package subr-x)

(use-package spinner
  :ensure t
  :defer t)

(use-package which-key
  :ensure t
  :defer t
  :diminish which-key-mode)
(which-key-mode t)

(provide 'init-utils)
