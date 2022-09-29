(use-package s
  :ensure t)

(defun customize-save-default (symbol value)
  (unless (boundp symbol)
    (customize-save-variable symbol value)))

(customize-save-default 'default-theme 'zenburn)

;; disable bold font globally
(setq set-face-ignore-attributes '(:weight :height))

(defadvice set-face-attribute
    (before ignore-attributes (face frame &rest args) activate)
  (unless (and (eq default-theme 'solarized-dark)
               (or (s-starts-with-p "sml" (symbol-name face))   ;; solarized-dark sml is an exception
                   (s-starts-with-p "ivy" (symbol-name face)))) ;; solarized-dark ivy is an exception
    (setq args
          (apply 'nconc
                 (mapcar (lambda (i)
                           (let ((attribute (nth i args))
                                 (value (nth (1+ i) args)))
                             (if (not (memq attribute
                                            set-face-ignore-attributes))
                                 (list attribute value))))
                         (number-sequence 0 (1- (length args)) 2))))))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(when (>= emacs-major-version 25) (horizontal-scroll-bar-mode -1))
(blink-cursor-mode 0)
(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq split-height-threshold nil)
(setq ring-bell-function 'ignore)

;; color theme
(use-package zenburn-theme
  :ensure t
  :config
  (setq zenburn-override-colors-alist
        '(
          ("zenburn-fg+1"     . "#FCFCEC")
          ("zenburn-fg"       . "#D0D0C0")
          ("zenburn-fg-1"     . "#626252")
          ("zenburn-bg-2"     . "#030303")
          ("zenburn-bg-1"     . "#2E2E2E")
          ("zenburn-bg-05"    . "#3B3B3B")
          ("zenburn-bg"       . "#424242")
          ("zenburn-bg+05"    . "#4C4C4C")
          ("zenburn-bg+1"     . "#525252")
          ("zenburn-bg+2"     . "#626262")
          ))
  (setq zenburn-colors-alist
        (append zenburn-default-colors-alist zenburn-override-colors-alist))
  )

(use-package solarized-theme
  :ensure t
  :defer t
  :config
  (setq solarized-use-less-bold t))


(load-theme default-theme t)

(fringe-mode 5)

(when (eq default-theme 'zenburn)
  (set-cursor-color "#aaaaaa"))

(use-package smart-mode-line
  :ensure t
  :defer t
  :config
  (setq sml/theme nil
        sml/shorten-modes t
        sml/mode-width 'right
        sml/shorten-directory t
        sml/name-width '(10 . 35)
        sml/show-frame-identification nil))
(sml/setup)

;; display batter usage for notebook
(use-package battery
  :defer t
  :config
  (setq battery-mode-line-format "[%b%p%%] "))

;; display time in 24hr format
(use-package time
  :defer t
  :config
  (setq display-time-24hr-format t
        display-time-string-forms (delete 'load display-time-string-forms)))

(display-battery-mode 1)
(display-time)

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(use-package subr-x)

(use-package spinner
  :ensure t
  :defer t)

(use-package which-key
  :ensure t
  :defer t
  :diminish which-key-mode)
(which-key-mode t)

(use-package smex
  :ensure t
  :bind ("M-x" . smex)
  :config
  ;; auto update smex cache after load a file
  (defun smex-update-after-load (unused)
    (when (boundp 'smex-cache)
      (smex-update)))
  (add-hook 'after-load-functions 'smex-update-after-load))

;; bookmark+
(defun bookmark-load-if-not ()
  (use-package bookmark+
    :ensure t)
  (if (and (not bookmarks-already-loaded) (file-readable-p bookmark-default-file))
      (bookmark-load bookmark-default-file)))

;; Auto save bookmark to file every 8 modifications
(setq bookmark-save-flag 8)

(provide 'init-basic)
