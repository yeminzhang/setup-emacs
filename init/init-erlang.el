(add-to-list 'load-path erlang-emacs-tools-dir)
(require 'erlang-start)


;;(add-to-list 'load-path "/home/eyemzha/.emacs.d/distel/elisp")
;;(require 'distel)
;;(distel-setup)

(setq distel-tags-compliant nil)

(add-hook 'erlang-mode-hook #'(lambda () (company-mode 1)))

(defun dmx-reload ()
(interactive)
(shell-command "scp ~/dmx/dmxc/out/opt/dmxc/lib/erlang/lib/dmxc-4.0/ebin/* root@SC-2-1:/persistent/dmx/active/dmxc/dev_patches/" nil)
(erl-reload-modules "dmxc1@SC-2-1"))

(defun erlang-find-tag-under-point ()
(interactive)
(let
    ((project-id (project-get-id buffer-file-name)))
  (if project-id 
      (progn
	(setq tags-file-name (concat (project-get-attribute project-id :root-dir) "TAGS"))
	(erlang-find-tag (erlang-find-tag-default))
	))))

(provide 'init-erlang)
