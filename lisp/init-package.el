(require 'package)
;;(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://1.15.88.122/melpa/") t)

;; install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; diminish is used by use-package, so we put it here before any other packages
(use-package diminish
  :ensure t)

(provide 'init-package)
