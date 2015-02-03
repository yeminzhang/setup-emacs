;;(setq emacs-configuration-root-dir "~/.emacs.d/")
;;(add-to-list 'load-path emacs-configuration-root-dir)

;; Remote node information, for nodeconnect to use
;; Minimal information about nodes.
;; More info in ~/.ssh/config

(setq keyboard-layout "sv")

(setq ssh-tunnel-host-list '("mylab" "hub"))

;; Configure erlang
(setq erlang-emacs-tools-dir "~/bsp/dmxc/erlang-otp/lib/tools/emacs")

;; Configure updatedb options
(setq updatedb-options "--localpaths=/etc --netpaths=/home/eyemzha --output=/home/eyemzha/locatedb --prunepaths=\"/home/eyemzha/.snapshot\"")

(provide 'env)
