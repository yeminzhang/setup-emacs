;;(require 'helm-config)

;; key bindings
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C--") 'undo)
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

;; Tabsn
(setq-default indent-tabs-mode 1)
(setq-default tab-width 8)
(setq-default c-basic-offset 8)

;; vim commands for convenience
;;(fset 'delete-whole-line "\C-a\C-k\C-k")
;;(global-set-key (kbd "C-m dd") 'delete-whole-line)
(global-set-key (kbd "C-x g") 'beginning-of-buffer)
(global-set-key (kbd "C-x G") 'end-of-buffer)
(fset 'copy-whole-line "\C-a\C- \C-e\M-w")
(global-set-key (kbd "C-x y") 'copy-whole-line)

(setq x-select-enable-clipboard t)

;; ido
(require 'ido)
(ido-mode 'both)
(setq ido-enable-flex-matching t)

(add-hook 'ido-minibuffer-setup-hook #'(lambda () (local-set-key (kbd "ö") 'ido-exit-minibuffer)))

;; smex
(require 'smex)
(global-set-key (kbd "C-ö") 'smex)

;; auto update smex cache after load a file
(defun smex-update-after-load (unused)
(when (boundp 'smex-cache)
(smex-update)))

(add-hook 'after-load-functions 'smex-update-after-load)

;; tramp
(require 'tramp)
(setq password-cache-expiry nil)
(setq tramp-default-method "scp")
(setq tramp-default-user "root")
(setq recentf-auto-cleanup 'never)

    (defun chomp (str)
      "Chomp leading and tailing whitespace from STR."
      (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                           str)
        (setq str (replace-match "" t t str)))
      str)


(defun my-helm-locate-init ()
  "Initialize async locate process for `helm-source-locate'."
  (let* ((locate-is-es (string-match "^es" helm-locate-command))
         (real-locate (string-match "^locate" helm-locate-command))
         (case-sensitive-flag (if locate-is-es "-i" ""))
         (ignore-case-flag (if (or locate-is-es
                                   (not real-locate)) "" "-i"))
         process-connection-type)
    (prog1
        (start-process-shell-command
         "locate-process" helm-buffer
         (format helm-locate-command
                 (case helm-locate-case-fold-search
                   (smart (let ((case-fold-search nil))
                            (if (string-match "[A-Z]" helm-pattern)
                                case-sensitive-flag
                                ignore-case-flag)))
                   (t (if helm-locate-case-fold-search
                          ignore-case-flag
                          case-sensitive-flag)))
				 (replace-regexp-in-string " " ".*" args)))
      (set-process-sentinel
       (get-process helm-buffer)
       #'(lambda (process event)
           (if (string= event "finished\n")
               (with-helm-window
                 (setq mode-line-format
                       '(" " mode-line-buffer-identification " "
                         (line-number-mode "%l") " "
                         (:eval (propertize
                                 (format "[Locate Process Finish- (%s results)]"
                                         (max (1- (count-lines
                                                   (point-min) (point-max))) 0))
                                 'face 'helm-grep-finish))))
                 (force-mode-line-update))
               (helm-log "Error: Locate %s"
                         (replace-regexp-in-string "\n" "" event))))))))



(defun my-new-helm-locate-init ()
  "Initialize async locate process for `helm-source-locate'."
  (let* ((locate-is-es (string-match "\\`es" helm-locate-command))
         (real-locate (string-match "\\`locate" helm-locate-command))
         (case-sensitive-flag (if locate-is-es "-i" ""))
         (ignore-case-flag (if (or locate-is-es
                                   (not real-locate)) "" "-i"))
         process-connection-type
         (args (split-string helm-pattern " ")))
    (prog1
        (start-process-shell-command
         "locate-process" helm-buffer
         (format helm-locate-command
                 (cl-case helm-locate-case-fold-search
                   (smart (let ((case-fold-search nil))
                            (if (string-match "[A-Z]" helm-pattern)
                                case-sensitive-flag
                              ignore-case-flag)))
                   (t (if helm-locate-case-fold-search
                          ignore-case-flag
                        case-sensitive-flag)))
                  ;; The pattern itself.
		 (shell-quote-argument (mapconcat 'identity args ".*"))))
;;(shell-quote-argument "dmxc.*capturing")))
;;		  (replace-regexp-in-string " " ".*" args)))
;;                 (concat
;;                  (shell-quote-argument (car args)) " "
                  ;; Possible locate args added
                  ;; after pattern, don't quote them.
  ;;                (mapconcat 'identity (cdr args) " "))))
      (set-process-sentinel
       (get-buffer-process helm-buffer)
       #'(lambda (_process event)
           (if (string= event "finished\n")
               (with-helm-window
                 (setq mode-line-format
                       '(" " mode-line-buffer-identification " "
                         (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                         (:eval (propertize
                                 (format "[Locate Process Finish- (%s results)]"
                                         (max (1- (count-lines
                                                   (point-min) (point-max)))
                                              0))
                                 'face 'helm-locate-finish))))
                 (force-mode-line-update))
             (helm-log "Error: Locate %s"
                       (replace-regexp-in-string "\n" "" event))))))))




(defvar my-helm-source-locate
  `((name . "Locate")
    (init . helm-locate-set-command)
    (candidates-process . my-new-helm-locate-init)
    (type . file)
    (requires-pattern . 3)
    (history . ,'helm-file-name-history)
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (candidate-number-limit . 9999)
    (mode-line . helm-generic-file-mode-line-string))
  "Find files matching the current input pattern with locate.")

(defvar my-new-helm-source-locate
  `((name . "Locate")
    (init . helm-locate-set-command)
    (candidates-process . my-new-helm-locate-init)
    (type . file)
    (requires-pattern . 3)
    (history . ,'helm-file-name-history)
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (candidate-number-limit . 9999)
;;    (no-matchplugin)
    (mode-line . helm-generic-file-mode-line-string))
  "Find files matching the current input pattern with locate.")


;; file a file
(global-set-key (kbd "C-x C-f")
  (lambda() (interactive)
    (helm
     :prompt "Switch to: "
     :candidate-number-limit 25                 ;; up to 25 of each
     :sources
     '(;;my-c-source-google-suggest
	   helm-source-files-in-current-dir ;; current dir
		helm-c-source-recentf               ;; recent files
;;	   helm-source-find-files
        helm-source-locate))))            ;; use 'locate'

(setq helm-dir-db-file "/home/eyemzha/.emacs.d/allfolder")


(defvar my-helm-source-find-dir
  `((name . "Go to dir:")
    (init . (lambda ()
	      (with-current-buffer (helm-candidate-buffer 'global)
		(insert-file-contents helm-dir-db-file))))
    (candidates-in-buffer)
    (keymap . ,helm-generic-files-map)
    (filtered-candidate-transformer . (lambda (candidates sources)
                                        (reverse candidates)))
    (candidate-number-limit . 9999)
    (action . (lambda (candidate)
                (find-file candidate))))
  "Helm source for Go to Directory.")

(defun my-helm-find-dir ()
(interactive)
(helm
   :prompt "Go to dir: "
   :candidate-number-limit 25                 ;; up to 25 of each
   :sources
   '(
	 my-helm-source-find-dir
 )))

(global-set-key (kbd "C-x d") 'my-helm-find-dir)

(defun updatedb ()
(interactive)
(shell-command "sudo updatedb"))

(defun updatedir-db ()
(interactive)
(shell-command (concat "find / -type d 2>/dev/null 1>" helm-dir-db-file)))


(set-language-environment "UTF-8")

(provide 'init-edit)
