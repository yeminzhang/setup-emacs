(package-install-when-not-exist '(yasnippet helm-c-yasnippet ))

(defun readelf (filename)
  (interactive (list (ido-read-file-name "file: ")))
  (shell-command (concat "readelf -a " filename)))

;; configure semantic
(require 'semantic)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)

(defun get-mode-map (mode)
  (if (eq mode 'cc-mode) 'c++-mode-map)
  )

;; Package: yasnippet
(require 'yasnippet)
(yas-global-mode 1)

(provide 'init-programming)
