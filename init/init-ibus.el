
;; ibus
(add-to-list 'load-path "~/.emacs.d/ibus-el-0.3.2/")
(require 'ibus)
(setq ibus-mode-local t)
;;(add-hook 'after-init-hook 'ibus-mode-on)
(global-set-key (kbd "Ö") 'toggle-ibus-mode)

;;(setq ibus-mode-on-p nil)

(defun toggle-ibus-mode ()
(interactive)
(if ibus-mode (ibus-mode-off)
(progn
(ibus-mode-on)
(ibus-check-current-buffer)
(ibus-disable)
(ibus-enable)
)))


 (ibus-define-common-key (kbd "C-j") nil)
 (ibus-define-common-key (kbd "ö") nil)
;; (ibus-define-common-key (kbd "SPC") nil)
;; (ibus-define-common-key 'kp-space nil)
;; (ibus-define-common-key ?j nil)

;;(ibus-define-preedit-key (kbd "SPC") t)
 (ibus-define-preedit-key (kbd "C-j") nil)
;; (ibus-define-preedit-key (kbd "SPC") t)
 (ibus-define-preedit-key (kbd "ö") nil)
;; (ibus-define-preedit-key ?ö nil)
;; (ibus-define-preedit-key ?j nil)

 (setq ibus-prediction-window-position t)

(ibus-mode-on)
(define-key ibus-mode-preedit-map (kbd "SPC") 'ido-switch-buffer)
;;(ibus-update-key-bindings)
(ibus-mode-off)

(provide 'init-ibus)
