(defun project-get-id (filename)
  (if (eq nil filename) nil
    (progn
      (setq expanded-filename (expand-file-name filename))
      (setq local-project-list project-list)
      (setq result nil)
      (while local-project-list
	(setq project (pop local-project-list))
	(setq project-root-dir (expand-file-name (plist-get project :root-dir)))
	(if (string= (substring expanded-filename 0 (length project-root-dir)) project-root-dir)
	    (setq result (plist-get project :id))
	  ))
      result
      )))

(defun project-get-attribute (project-id attribute-name)
  (progn
    (setq local-project-list project-list)
    (setq result nil)
    (while local-project-list
      (setq project (pop local-project-list))
      (setq id (plist-get project :id))
      (if (string= id project-id)
	  (setq result (plist-get project attribute-name))
	))
    result
    ))

(defun project-compile ()
  (interactive)
  (progn
    (setq project-id (project-get-id buffer-file-name))
    (if (eq nil project-id) (recompile)
      (progn
	(setq configured-command (project-get-attribute project-id :compile-command))
	(setq root-dir (project-get-attribute project-id :root-dir))
	(setq command (if (eq nil configured-command) "make" configured-command))
	(save-some-buffers 1)
	(setq current-project-id project-id)
	(compile (concat "cd " root-dir "; " command))
	))
    ))

;; Close the compilation window if there was no error at all.
(setq compilation-exit-message-function
      (lambda (status code msg)
	;; If M-x compile exists with a 0
	(when (and (eq status 'exit) (zerop code))
	  ;; then bury the *compilation* buffer, so that C-x b doesn't go there
  	  (bury-buffer "*compilation*")
  	  ;; and return to whatever were looking at before
  	  (replace-buffer-in-windows "*compilation*")
	  ;; Update TAGS if it belongs to a project
	  (project-update-tags current-project-id))
	;; Always return the anticipated result of compilation-exit-message-function
  	(cons msg code)))


;; Do this when just create the project on disk. Otherwise project-find-tag won't work
(defun project-update-tags (project-id)
  (interactive (list (project-get-id buffer-file-name)))
    (unless (eq nil project-id)
     (shell-command (concat "cd " (project-get-attribute project-id :root-dir) "; " (project-get-attribute project-id :tag-command)))))

(defun project-find-tag ()
  (interactive)
  (let
      ((project-id (project-get-id buffer-file-name)))
    (unless (eq nil project-id)
      (progn
	(setq tags-file-name (concat (project-get-attribute project-id :root-dir) "TAGS"))
	(if (string= "erlang" (project-get-attribute project-id :type)) (erlang-find-tag-under-point))
	))))

(global-set-key (kbd "<f5>") 'project-compile)
(global-set-key (kbd "M-.") 'project-find-tag)
(global-set-key (kbd "M-,") 'pop-tag-mark)

(provide 'project)
