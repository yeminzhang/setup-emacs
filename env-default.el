;; To overrite the default value, set those variables in
;; ~/.emacs.d/env.el

;; Either sv or en
(setq keyboard-layout "en")

;; Should be Host in ~/.ssh/config. A ssh tunnel will
;; setup automatically if you add to the list
(setq ssh-tunnel-host-list '())

;; If you want to add ede project, add the following code in env-default.el
;;(require 'ede)
;;(ede-cpp-root-project "<Project-Name>"
;;                      :file "<project-root-path-file>"
;;                      :include-path '("/include1"))
