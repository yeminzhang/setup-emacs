(defun readelf (filename)
  (interactive (list (ido-read-file-name "file: ")))
  (shell-command (concat "readelf -a " filename)))

;; configure semantic
(require 'semantic)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)

;; Package: yasnippet
(require 'yasnippet)
(yas-global-mode 1)

(provide 'init-programming)
