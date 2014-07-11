(add-to-list 'load-path "/home/eyemzha/opt/lib/erlang/lib/tools-2.6.12/emacs")
(setq erlang-root-dir "/home/eyemzha/opt")
(add-to-list 'exec-path "/home/eyemzha/opt/bin")
(require 'erlang-start)


;;(add-to-list 'load-path "/home/eyemzha/.emacs.d/distel/elisp")
;;(require 'distel)
;;(distel-setup)

(setq distel-tags-compliant nil)

;;TODO
(defun essim-connect ()
(interactive)
(async-shell-command "/home/eyemzha/essim/my_start_essim.sh /home/eyemzha/essim/ /home/eyemzha/dmx/"))

;;TODO
(defun dmx-start ()
(interactive)
;;(async-shell-command "sudo /home/eyemzha/essim/start_essim.sh /home/eyemzha/essim/ /home/eyemzha/dmx/"))
;;(start-process "my-p-name" "my-buffer" "sudo" "/home/eyemzha/essim/start_essim.sh" "/home/eyemzha/essim/" "/home/eyemzha/dmx/"))
(shell-command "sudo /home/eyemzha/essim/start_essim.sh /home/eyemzha/essim/ /home/eyemzha/dmx/ &" "mybuffer"))


(defun dmx-compile ()
(interactive)
(save-some-buffers 1)
(recompile)
)


(defun dmx-reload ()
(interactive)
(shell-command "scp /home/eyemzha/dmx/dmxc/out/opt/dmxc/lib/erlang/lib/dmxc-4.0/ebin/* root@SC-2-1:/persistent/dmx/active/dmxc/dev_patches/" nil)
(erl-reload-modules "dmxc1@SC-2-1"))

;;(define-key erlang-mode-map (kbd "<f5>") 'dmx-compile)
;;(define-key erlang-mode-map (kbd "<f6>") 'dmx-reload)

(provide 'init-erlang)
