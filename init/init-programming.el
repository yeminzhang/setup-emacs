(require-packages '(yasnippet helm-c-yasnippet ))

(defun readelf (filename)
  (interactive (list (ido-read-file-name "file: ")))
  (shell-command (concat "readelf -a " filename)))

;; configure semantic
(require 'semantic)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)
(require 'helm-semantic)

(defun get-mode-map (mode)
  (if (eq mode 'cc-mode) 'c++-mode-map)
  )

;; Package: yasnippet
(require 'yasnippet)
(yas-global-mode 1)

(define-key yas-minor-mode-map [backtab] 'yas-expand)

(define-key yas-minor-mode-map [(tab)]        nil)
(define-key yas-minor-mode-map (kbd "TAB")    nil)
(define-key yas-minor-mode-map (kbd "<tab>")  nil)


(provide 'init-programming)
