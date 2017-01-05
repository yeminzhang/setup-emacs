(require-packages '(persp-mode))

;; desktop save
(setq desktop-path (list user-emacs-directory))
(desktop-save-mode 1)
(setq desktop-restore-eager t
      desktop-restore-frames nil)
;;(setq desktop-files-not-to-save "^$")

(add-hook 'desktop-after-read-hook 'persp-mode)
(add-hook 'desktop-not-loaded-hook 'persp-mode)
(add-hook 'desktop-no-desktop-file-hook 'persp-mode)

(setq persp-keymap-prefix "^Cs")

(after-load 'persp-mode
  (add-to-list 'persp-before-switch-functions 'session-save-last-used-name)
  (add-to-list 'persp-activated-functions 'session-save-currently-used-name)
  (setq persp-nil-hidden t
        persp-filter-save-buffers-functions nil)
  (add-to-list 'persp-filter-save-buffers-functions
               (lambda
                 (b)
                 (string-prefix-p "*magit"
                                  (buffer-name b))))
;;
;; patch
(defun persp-switch-to-buffer (buffer-or-name &optional norecord force-same-window)
  "Switch to buffer, read buffer with restriction to current perspective."
  (interactive (list
                (let ((*persp-restrict-buffers-to* 0)
                      (completing-read-function 'ido-completing-read)
                      persp-restrict-buffers-to-if-foreign-buffer)
                  (if persp-mode
                      (persp-read-buffer "buffer: " (other-buffer) t)
                    (read-buffer "buffer: " (other-buffer) t)))))
  (when (and buffer-or-name
             (persp-get-buffer-or-null (get-buffer buffer-or-name)))
    (switch-to-buffer buffer-or-name norecord force-same-window)))

;; patch to remove default prompt
(defun* persp-read-buffer (prompt &optional default require-match predicate multiple (default-mode t))
  "Read buffers with restriction."
  (setq persp-disable-buffer-restriction-once nil)
  (when default
    (unless (stringp default)
      (if (and (bufferp default) (buffer-live-p default))
          (setq default (buffer-name default))
        (setq default nil))))
  (when prompt
    (setq prompt (car (split-string prompt ": *$" t))))

  (let ((buffer-names (mapcar #'buffer-name
                              (delete-if #'persp-buffer-filtered-out-p
                                         (persp-buffer-list-restricted))))
        retlst)
    (macrolet ((call-pif ()
                         `(funcall persp-interactive-completion-function
                                   (concat prompt
                                           (and retlst
                                                (concat "< " (mapconcat #'identity retlst " ") " >"))
                                           ": ")
                                   buffer-names predicate require-match nil nil default)))
      (if multiple
          (let ((done_str "[>done<]") (not-finished default-mode)
                exit-minibuffer-function mb-local-key-map
                (push-keys (alist-get 'push-item persp-read-multiple-keys))
                (pop-keys (alist-get 'pop-item persp-read-multiple-keys))
                (toggle-filter-keys (alist-get 'toggle-persp-buffer-filter persp-read-multiple-keys))
                push-keys-backup pop-keys-backup toggle-filter-keys-backup)
            (while (member done_str buffer-names)
              (setq done_str (concat ">" done_str)))
            (let ((persp-minibuffer-setup #'(lambda ()
                                              (setq mb-local-key-map (current-local-map))
                                              (when (keymapp mb-local-key-map)
                                                (unless exit-minibuffer-function
                                                  (setq exit-minibuffer-function
                                                        (or (lookup-key mb-local-key-map (kbd "RET"))
                                                            persp-read-multiple-exit-minibuffer-function)))
                                                (unless push-keys-backup
                                                  (setq push-keys-backup
                                                        (lookup-key mb-local-key-map push-keys)))
                                                (define-key mb-local-key-map push-keys
                                                  #'(lambda () (interactive)
                                                      (setq not-finished 'push)
                                                      (funcall exit-minibuffer-function)))
                                                (unless pop-keys-backup
                                                  (setq pop-keys-backup
                                                        (lookup-key mb-local-key-map pop-keys)))
                                                (define-key mb-local-key-map pop-keys
                                                  #'(lambda () (interactive)
                                                      (setq not-finished 'pop)
                                                      (funcall exit-minibuffer-function)))
                                                (unless toggle-filter-keys-backup
                                                  (setq toggle-filter-keys-backup
                                                        (lookup-key mb-local-key-map toggle-filter-keys)))
                                                (define-key mb-local-key-map toggle-filter-keys
                                                  #'(lambda () (interactive)
                                                      (setq not-finished 'toggle-filter)
                                                      (funcall exit-minibuffer-function))))))
                  cp)
              (unwind-protect
                  (progn
                    (when (and default (not (member default buffer-names)))
                      (setq default nil))
                    (add-hook 'minibuffer-setup-hook persp-minibuffer-setup)
                    (while not-finished
                      (setq cp (call-pif))
                      (case not-finished
                        (push
                         (when (and cp (member cp buffer-names))
                           (if retlst
                               (when (string= cp done_str)
                                 (setq not-finished nil))
                             (push done_str buffer-names))
                           (when not-finished
                             (if (eq 'reverse multiple)
                                 (setq retlst (append retlst (list cp)))
                               (push cp retlst))
                             (setq buffer-names (delete cp buffer-names)
                                   default done_str)))
                         (when not-finished
                           (setq not-finished default-mode)))
                        (pop
                         (let ((last-item (pop retlst)))
                           (unless retlst (setq buffer-names (delete done_str buffer-names)
                                                default nil))
                           (when last-item
                             (push last-item buffer-names)))
                         (setq not-finished default-mode))
                        (toggle-filter
                         (setq persp-disable-buffer-restriction-once
                               (not persp-disable-buffer-restriction-once))
                         (setq buffer-names (delete-if
                                             #'(lambda (bn) (member bn retlst))
                                             (mapcar #'buffer-name
                                                     (if persp-disable-buffer-restriction-once
                                                         (funcall persp-buffer-list-function)
                                                       (delete-if #'persp-buffer-filtered-out-p
                                                                  (persp-buffer-list-restricted))))))
                         (setq not-finished default-mode))
                        (t
                         (when (and cp (not (string= cp done_str)) (member cp buffer-names))
                           (push cp retlst))
                         (setq not-finished nil))))
                    retlst)
                (remove-hook 'minibuffer-setup-hook persp-minibuffer-setup)
                (when (keymapp mb-local-key-map)
                  (when (lookup-key mb-local-key-map push-keys)
                    (define-key mb-local-key-map push-keys push-keys-backup))
                  (when (lookup-key mb-local-key-map pop-keys)
                    (define-key mb-local-key-map pop-keys pop-keys-backup))
                  (when (lookup-key mb-local-key-map toggle-filter-keys)
                    (define-key mb-local-key-map toggle-filter-keys toggle-filter-keys-backup)))
                (setq persp-disable-buffer-restriction-once nil))))
        (call-pif)))))
)

(add-hook 'persp-mode-hook
          (lambda ()
            (when (bound-and-true-p session-currently-used)
              (persp-switch session-currently-used))))

(defun session-kill ()
  (interactive)
  (let ((completing-read-function 'ido-completing-read))
    (call-interactively 'persp-kill)))

(defun session-switch ()
  (interactive)
  (let ((completing-read-function 'ido-completing-read))
    (call-interactively 'persp-switch)))

(defun session-rename ()
  (interactive)
  (call-interactively 'persp-rename))

(defun session-next()
  (interactive)
  (call-interactively 'persp-next))

(defun session-previous()
  (interactive)
  (call-interactively 'persp-prev))

(defun session-add-buffer ()
  (interactive)
  (let ((completing-read-function 'ido-completing-read))
    (call-interactively 'persp-add-buffer)))

(defun session-save-last-used-name (persp frame)
  (setq session-last-used persp-last-persp-name))

(defun session-save-currently-used-name (frame-or-window)
  (setq session-currently-used persp-last-persp-name))

(add-to-list 'desktop-globals-to-save 'session-currently-used)

(defun session-last()
  (interactive)
  (when (bound-and-true-p session-last-used)
    (persp-switch session-last-used)))

(provide 'init-session)
