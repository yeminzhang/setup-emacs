;;(setq emacs-configuration-root-dir "~/.emacs.d/")
;;(add-to-list 'load-path emacs-configuration-root-dir)

;; Remote node information, for nodeconnect to use
;; Minimal information about nodes.
;; More info in ~/.ssh/config
(setq machine-list
'(
(:id "gateway" :password "eyemzha")
(:id "js" :password "js")
(:id "js2" :password "js")
(:id "iptb-console" :password "auto")
(:id "mylab-dmx1" :password "rootroot")
(:id "mylab-dmx2" :password "rootroot")
))

;; Configure erlang
(setq erlang-root-dir "~/opt/")

(provide 'env)
