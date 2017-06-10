(use-package helm
  :defer t
  :ensure t
  :bind (:map helm-map
              (";" . helm-maybe-exit-minibuffer)
              )
  :config
  (setq helm-input-idle-delay 0.01
        helm-full-frame nil
        helm-split-window-preferred-function 'helm-split-window-my-fn)

  (defun helm-run (ARG prompt sources)
    (helm
     :prompt prompt
     :candidate-number-limit 25                 ;; up to 25 of each
     :sources sources))

  (defun helm-split-window-my-fn (window)
    (let (split-width-threshold)
      (if (and (fboundp 'window-in-direction)
               ;; Don't try to split when starting in a minibuffer
               ;; e.g M-: and try to use helm-show-kill-ring.
               (not (minibufferp helm-current-buffer)))
          (if (or (one-window-p t)
                  helm-split-window-in-side-p)
              (split-window
               (selected-window) nil 'right)
            ;; If more than one window reuse one of them.
            (other-window-for-scrolling))
        (split-window-sensibly window))))
  )

(use-package helm-mode
  :defer t
  :diminish helm-mode)

(use-package helm-files
  :after helm-mode
  :config
  (global-set-key (kbd "C-x C-f")  'helm-find-file)
  (global-set-key (kbd "C-x C-d")  'helm-find-dir)
  ;; Show full file-path in helm result
  (setq helm-ff-transformer-show-only-basename nil)
  (defclass helm-files-in-current-dir-source-no-fuzzy (helm-files-in-current-dir-source)
    (
     (fuzzy-match :initform nil)
     ))
  (setq helm-source-files-in-current-dir
        (helm-make-source "Files from Current Directory"
            helm-files-in-current-dir-source-no-fuzzy))
  (setq helm-source-recentf-files-only (copy-tree helm-source-recentf))
  (setf (nth 0 helm-source-recentf-files-only) '(name . "Recent files excluing directories"))
  (setf (nth 3 (nth 2 helm-source-recentf-files-only)) '(delete-dups
                                                         (mapcar (lambda (file)
                                                                   (if (s-ends-with-p "/" file) "" file))
                                                                 recentf-list)))

  (setq helm-source-recentf-dirs-only (copy-tree helm-source-recentf))
  (setf (nth 0 helm-source-recentf-dirs-only) '(name . "Recent directories"))
  (setf (nth 3 (nth 2 helm-source-recentf-dirs-only)) '(delete-dups
                                                        (mapcar (lambda (file)
                                                                  (if (s-ends-with-p "/" file) file ""))
                                                                recentf-list)))
  (defun helm-find-file (ARG)
    (interactive "P")
    (let (
          (helm-locate-command (make-locate-command ARG))
          )
      (helm-run ARG
                "Open file: "
                '(  helm-source-files-in-current-dir
                    helm-source-recentf-files-only
                    helm-source-locate-files))))

  (defun helm-find-dir (ARG)
    (interactive "P")
    (let (
          (helm-locate-command (make-locate-command ARG))
          )
      (helm-run ARG
                "Open dir: "
                '(helm-source-recentf-dirs-only
                  helm-source-locate-dirs))))
  )

(use-package helm-locate
  :after helm-mode
  :config
  (setq helm-source-locate-dirs (copy-tree helm-source-locate))
  (setf (nth 1 (nth 8 helm-source-locate-dirs)) 'keep-only-dirs)

  (setq helm-source-locate-files (copy-tree helm-source-locate))
  (let ((candidate-transformer (nth 8 helm-source-locate-files)))
    (setcdr candidate-transformer (cons 'keep-only-files (cdr candidate-transformer))))
  )

(use-package helm-buffers
  :after helm-mode
  :config
  (setq helm-buffers-fuzzy-matching t)
  ;; This is a patch to prevent helm from sorting the buffer
  ;; list when narrowing
  (defun helm-buffers-sort-transformer (candidates _source)
    candidates)
  (global-set-key (kbd "C-x C-b")  'helm-buffers-list))


(use-package helm-ring
  :after helm-mode
  :config
  (global-set-key (kbd "M-y")  'helm-show-kill-ring))

(use-package helm-swoop
  :ensure t
  :after helm-mode
  :config
  ;; C-s in a buffer: open helm-swoop with empty search field
  (setq helm-swoop-pre-input-function (lambda () nil))

  (defun tl/helm-swoop-search (search-direction)
    (if (boundp 'helm-swoop-pattern)
        (if (equal helm-swoop-pattern "")
            (previous-history-element 1)
          (call-interactively search-direction))
      (call-interactively 'search-direction)))

  ;; C-s in helm-swoop with empty search field: activate previous search.
  ;; C-s in helm-swoop with non-empty search field: go to next match.
  (defun tl/helm-swoop-C-s ()
    (interactive)
    (tl/helm-swoop-search 'helm-next-line))

  (defun tl/helm-swoop-C-r ()
    (interactive)
    (tl/helm-swoop-search 'helm-previous-line))

  (global-set-key (kbd "C-s")  'helm-swoop)
  (global-set-key (kbd "C-r")  'helm-swoop)
  :bind (
         :map helm-swoop-map
         ("C-s" . tl/helm-swoop-C-s)
         ("C-r" . tl/helm-swoop-C-r)
         )
  )

(use-package helm-config
  :after helm
  :init
  (setq helm-command-prefix-key "C-c h"))

(use-package helm-projectile
  :ensure t
  :after projectile helm-mode
  :config
  (helm-projectile-on)
  (define-key projectile-mode-map (kbd "C-c p g") 'helm-projectile-grep)
  (setq helm-projectile-fuzzy-match nil))

;;(helm-mode 1)

(provide 'init-helm)
