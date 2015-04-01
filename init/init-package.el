(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defun package-install-when-not-exist (packages)
  (dolist (package packages)
  (unless (package-installed-p package)
	(unless (boundp 'package-refreshed) (package-refresh-contents)(setq package-refreshed t))
	(package-install package))))

(provide 'init-package)
