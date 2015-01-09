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
(:id "mylab-dmx1" :password "rootroot" :prompt "SC-2-1")
(:id "mylab-dmx2" :password "rootroot" :prompt "SC-2-2")
))

;; Configure erlang
(setq erlang-root-dir "~/opt/")
;; Configure erlang targets

;; Configure project
;; should have following attributes
;;   :id bsp
;;   :type(optional) erlang
;;   :root-dir ~/dmx/dmxc/
;;   :compile-command(to be executed under root-dir. if ommit, then it is make) make
;;   :tag-filename(if ommit, then it is root-dir/TAGS) relative path to root-dir
(setq project-list
'(
(:id "dmxc" :type "erlang" :root-dir "~/bsp/dmxc/" :tag-command "etags src/*" :compile-command "../make-dmxc-remote")
(:id "cu" :type "other" :root-dir "~/bsp/capturing-unit/" :tag-command "etags src/*" :compile-command "../make-cu-remote")
))

;; Configure updatedb options
(setq updatedb-options "--localpaths=/etc --netpaths=/home/eyemzha --output=/home/eyemzha/locatedb --prunepaths=\"/home/eyemzha/.snapshot\"")

(provide 'env)
