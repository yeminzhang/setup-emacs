(require 'magit)

;; reservered for future use when emacs 24.4 is ready
;;(require 'magit-filenotify)


(eval-after-load 'info
  '(progn (info-initialize)
          (add-to-list 'Info-directory-list "/home/eyemzha/.emacs.d/magit/")))

(global-set-key (kbd "<f4>")
  (lambda() (interactive)
    (magit-status (magit-get-top-dir))))

(provide 'init-magit)
