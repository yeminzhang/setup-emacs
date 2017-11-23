(use-package compile
  :defer t
  :diminish compilation-in-progress
  :config
  (setq compilation-read-command nil
        compilation-ask-about-save nil
        compilation-buffer-name-function 'project-get-compilation-buffer-name
        compilation-finish-functions 'compile-autoclose
        compilation-skip-threshold 2
        compilation-auto-jump-to-first-error t
        compilation-scroll-output 'first-error)
  (set-display-buffer-other-window (rx bos "*compilation-"))
  (defvar compile--spinner (spinner-create 'rotating-line))

  (defadvice compile (after compile-start-spinner activate)
    (spinner-start compile--spinner))

  ;; Close the compilation window if there was no error at all.
  (defun compile-autoclose (buffer string)
    (spinner-stop compile--spinner)
    (when (string-match "finished" string)
      (bury-buffer buffer)
      (replace-buffer-in-windows buffer)))
  :bind (:map compilation-mode-map
              ("C-o" . other-window)
         )
  )

(use-package gud
  :defer t
  :config
  (defun gud-active-process ()
    (if (get-buffer-process gud-comint-buffer) t nil)
    )

  (defun gud-send-command (command)
    (if (gud-active-process)
        (comint-send-string gud-comint-buffer (concat command "\n"))
      ))

  (defun gud-save-window-configuration ()
    (setq window-configuration-before-gdb (current-window-configuration))
    )

  (defun gud-restore-window-configuration ()
    (set-window-configuration window-configuration-before-gdb)
    )
  )

(use-package linum-relative
  :ensure t
  :defer t
  :diminish linum-relative-mode
  :config
  (setq linum-relative-plusp-offset 1
        linum-relative-current-symbol ""))

;; highlight-parentheses
(use-package highlight-parentheses
  :ensure t
  :defer t
  :config
  (setq hl-paren-delay 0.2
        hl-paren-colors '("Springgreen3"
                          "IndianRed1"
                          "IndianRed3"
                          "IndianRed4")))

(use-package prog-mode
  :defer t
  :config
  (semantic-mode 1)
  (add-hook 'prog-mode-hook 'configure-programming-buffer-common)

  (defun configure-programming-buffer-common ()
    (setq show-trailing-whitespace t)
    (linum-mode t)
    (highlight-parentheses-mode t)
    ;; define word boundary
    (modify-syntax-entry ?_ "w")
    (modify-syntax-entry ?- "w")
    (local-set-key (kbd "C-.") 'er/expand-region)))

(defun readelf (filename)
  (interactive (list (ido-read-file-name "file: ")))
  (shell-command (concat "readelf -a " filename)))

;; expand region
(use-package expand-region
  :ensure t
  :defer t)
(setq expand-region-contract-fast-key ","
      expand-region-reset-fast-key "r")

;; Package: yasnippet
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode-on
  :config
  (yas-reload-all))

(use-package helm-c-yasnippet
  :ensure t
  :defer t)

;; TODO investigate how to use ycmd for auto complete
(use-package company
  :defer t
  :config
  (setq company-backends (delete 'company-semantic company-backends)))
;;  (push '(company-semantic :with company-yasnippet) company-backends))

;; which function mode
(which-function-mode t)
(use-package which-func
  :defer t
  :config
  (setq which-func-modes '(emacs-lisp-mode c++-mode c-mode)))

;; configure semantic
;; TODO investigate how to use ycmd for auto complete
(global-semanticdb-minor-mode -1)
(global-semantic-idle-scheduler-mode -1)

(defun format-buffer ()
  (interactive)
  (when c-buffer-is-cc-mode (cc-format-buffer)))

(provide 'init-programming)
