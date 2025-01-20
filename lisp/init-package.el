(require 'package)
;;(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("gnu"   . "http://mirrors.cloud.tencent.com/elpa/gnu/") t)
(add-to-list 'package-archives '("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/") t)

;; install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; diminish is used by use-package, so we put it here before any other packages
(use-package diminish
  :ensure t)

(provide 'init-package)
