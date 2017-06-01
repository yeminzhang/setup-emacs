(use-package helm
  :defer t
  :ensure t
  :bind (:map helm-map
              (";" . helm-maybe-exit-minibuffer)
              )
  :config
  (setq helm-input-idle-delay 0.01
        helm-full-frame nil
        helm-split-window-default-side 'other)
  (defun helm-run (ARG prompt sources)
    (helm
     :prompt prompt
     :candidate-number-limit 25                 ;; up to 25 of each
     :sources sources))
  )

(use-package helm-mode
  :defer t
  :diminish helm-mode)

(use-package helm-files
  :bind (("C-x C-f" . helm-find-file)
         ("C-x C-d" . helm-find-dir))
  :config
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
  :defer t
  :config
  (setq helm-source-locate-dirs (copy-tree helm-source-locate))
  (setf (nth 1 (nth 8 helm-source-locate-dirs)) 'keep-only-dirs)

  (setq helm-source-locate-files (copy-tree helm-source-locate))
  (let ((candidate-transformer (nth 8 helm-source-locate-files)))
    (setcdr candidate-transformer (cons 'keep-only-files (cdr candidate-transformer))))
  )

(use-package helm-buffers
  :defer t
  :config
  (setq helm-buffers-fuzzy-matching t)
  ;; This is a patch to prevent helm from sorting the buffer
  ;; list when narrowing
  (defun helm-buffers-sort-transformer (candidates _source)
    candidates)
  )

(use-package helm-swoop
  :ensure t
  :defer t
  :config
  ;; C-s in a buffer: open helm-swoop with empty search field
  (setq helm-swoop-pre-input-function (lambda () nil))

  (defun tl/helm-swoop-search (search-direction)
    (if (boundp 'helm-swoop-pattern)
        (if (equal helm-swoop-pattern "")
            (previous-history-element 1)
          ;;                (insert "haha")
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

  :bind (
         ("C-s" . helm-swoop)
         ("C-r" . helm-swoop)
         :map helm-swoop-map
         ("C-s" . tl/helm-swoop-C-s)
         ("C-r" . tl/helm-swoop-C-r)
         )
  )

(use-package helm-config
  :init
  (setq helm-command-prefix-key "C-c h"))

(helm-mode 1)

(provide 'init-helm)
