;; To overrite the default value, set those variables in
;; ~/.emacs.d/env.el

;; Either sv or en
(setq keyboard-layout "en")

;; Should be Host in ~/.ssh/config. A ssh tunnel will
;; setup automatically if you add to the list
(setq ssh-tunnel-host-list '())

;; Configure erlang
(setq erlang-emacs-tools-dir "")

;; Configure updatedb options
(setq updatedb-options "--localpaths=/etc --netpaths=/home/eyemzha --output=/home/eyemzha/locatedb --prunepaths=\"/home/eyemzha/.snapshot\"")
