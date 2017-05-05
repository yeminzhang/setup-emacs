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

;;(require 'helm-config)
(helm-mode 1)

(provide 'init-helm)
