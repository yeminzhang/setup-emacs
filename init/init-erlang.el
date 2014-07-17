(add-to-list 'load-path (concat erlang-root-dir "lib/erlang/lib/tools-2.6.12/emacs"))
(add-to-list 'exec-path (concat erlang-root-dir "bin"))
(require 'erlang-start)


;;(add-to-list 'load-path "/home/eyemzha/.emacs.d/distel/elisp")
;;(require 'distel)
;;(distel-setup)

(setq distel-tags-compliant nil)


(defun dmx-compile ()
(interactive)
(save-some-buffers 1)
(recompile)
)


(defun dmx-reload ()
(interactive)
(shell-command "scp ~/dmx/dmxc/out/opt/dmxc/lib/erlang/lib/dmxc-4.0/ebin/* root@SC-2-1:/persistent/dmx/active/dmxc/dev_patches/" nil)
(erl-reload-modules "dmxc1@SC-2-1"))

(defun erlang-find-tag-under-point ()
(interactive)
(erlang-find-tag (erlang-find-tag-default))
)

(add-hook 'erlang-mode-hook #'(lambda ()
(define-key erlang-mode-map (kbd "<f5>") 'dmx-compile)))
;;(define-key erlang-mode-map (kbd "<f6>") 'dmx-reload)

(add-hook 'erlang-mode-hook #'(lambda ()
(define-key erlang-mode-map (kbd "M-.") 'erlang-find-tag-under-point)))

(add-hook 'erlang-mode-hook #'(lambda ()
(define-key erlang-mode-map (kbd "M-,") 'pop-tag-mark)))

(provide 'init-erlang)
