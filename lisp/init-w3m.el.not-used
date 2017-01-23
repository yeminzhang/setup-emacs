(require 'w3m)
(require 'helm-w3m)

;; create new session with a complete empty history
(defun my-w3m-goto-url-new-session (url &optional reload charset post-data
				     referer)
  "Visit World Wide Web pages in a new session.
If you invoke this command in the emacs-w3m buffer, the new session
will be created by copying the current session.  Otherwise, the new
session will start afresh."
  (interactive
   (list (w3m-input-url nil nil w3m-new-session-url nil 'feeling-lucky)
	 current-prefix-arg
	 (w3m-static-if (fboundp 'universal-coding-system-argument)
	     coding-system-for-read)
	 nil ;; post-data
	 nil)) ;; referer
  (let (buffer)
	(progn
	  ;; Store the current position in the history structure.
	  (switch-to-buffer
	   (setq buffer (w3m-copy-buffer nil nil
					 w3m-new-session-in-background
					 'empty)))
	  (setq w3m-history nil)
	  (w3m-goto-url url
			(or reload
			    ;; When new URL has `name' portion, we have to
			    ;; goto the base url because generated buffer
			    ;; has no content at this moment.
			    (and (progn
				   (w3m-string-match-url-components url)
				   (match-beginning 8))
				 'redisplay))
			charset post-data referer)
	  ;; Delete useless newly created buffer if it is empty.
	  (w3m-delete-buffer-if-empty buffer))))


(defvar my-helm-c-google-suggest-default-function
  'my-helm-c-google-suggest-set-candidates
  "Default function to use in helm google suggest.")


(defun my-helm-c-google-suggest-set-candidates (&optional request-prefix)
  "Set candidates with result and number of google results found."
(progn
(setq suggestions (helm-c-google-suggest-set-candidates request-prefix))
(dolist (element suggestions)
(if (string= helm-input (cdr element))
(setq list-to-remove element)))
(delete list-to-remove suggestions)
(add-to-list 'suggestions list-to-remove)))


(defvar my-helm-c-source-google-suggest
  '((name . "Google Suggest")
    (candidates . (lambda ()
                    (funcall my-helm-c-google-suggest-default-function)))
    (action . (("Google Search" . helm-google-suggest-action)))
    (volatile)
    (requires-pattern . 3)
    (delayed)))

(defvar my-helm-source-w3m-bookmarks-new-session
  '((name . "W3m Bookmarks")
    (init . (lambda ()
              (setq helm-w3m-bookmarks-alist
                    (helm-html-bookmarks-to-alist
                     w3m-bookmark-file
                     helm-w3m-bookmark-url-regexp
                     helm-w3m-bookmarks-regexp))))
    (candidates . (lambda ()
                    (mapcar #'car helm-w3m-bookmarks-alist)))
    (filtered-candidate-transformer
     helm-adaptive-sort
     helm-highlight-w3m-bookmarks)
    (action . (("Browse Url" . (lambda (candidate) (helm-w3m-browse-bookmark candidate t)))))
    (persistent-action . (lambda (candidate)
                           (if current-prefix-arg
                               (helm-w3m-browse-bookmark candidate t)
                               (helm-w3m-browse-bookmark candidate nil t))))
    (persistent-help . "Open URL with emacs-w3m in new tab / \
C-u \\[helm-execute-persistent-action]: Open URL with Firefox"))
  "Needs w3m and emacs-w3m.

http://w3m.sourceforge.net/
http://emacs-w3m.namazu.org/")



(global-set-key (kbd "<f1>")
  (lambda() (interactive)
    (helm
     :prompt "Go to: "
     :candidate-number-limit 25                 ;; up to 25 of each
     :sources
     '(
	   my-helm-source-w3m-bookmarks-new-session
	   my-helm-c-source-google-suggest
	 ;;helm-source-w3m-bookmarks
	 ;;helm-c-source-google-suggest
        ))))


;; w3m
(setq w3m-new-session-in-background t)
(setq w3m-cookie-accept-bad-cookies t)
(setq w3m-show-graphic-icons-in-mode-line nil)
(require 'w3m-lnum)
(add-hook 'w3m-mode-hook 'w3m-lnum-mode)
;;(add-hook 'w3m-mode-hook 'view-mode)
(require 'w3m-load)
(setq w3m-use-cookies t)
(setq browse-url-browser-function 'my-w3m-goto-url-new-session)
(setq w3m-local-find-file-function nil)
(setq w3m-history-minimize-in-new-session t)
(require 'mime-w3m)
(add-hook 'w3m-fontify-after-hook 'w3m-set-buffer-name)
;;(add-hook 'w3m-display-hook 'w3m-check-history)
;;(add-hook 'w3m-display-hook 'w3m-dictionary-display)

(defun w3m-set-buffer-name ()
(progn
(if (> (length w3m-current-title) 40) (setq title-length 40) (setq title-length (length w3m-current-title)))
(unless (string= (buffer-name (current-buffer)) "*DICTIONARY*")
(rename-buffer (concat "*web*<" (substring w3m-current-title 0 title-length) ">")))))

(defun w3m-dictionary-display (url)
(if (string= (buffer-name (current-buffer)) "*DICTIONARY*")
(progn
;;(scroll-up-command 51)
(unless (string-match "#" url)(scroll-up-command 51))
;;(occur "sentence")
)))


;;todo
(defun w3m-check-history ()
(progn
(setq prev-p "")
(if (w3m-history-previous-link-available-p) (setq prev-p "0"))
(rename-buffer (concat prev-p "*web*<" (substring w3m-current-title 0 title-length) ">"))))

  (defadvice w3m-modeline-title (around my-w3m-modeline-title)
      "prevent original function from running; cleanup remnants"
      (setq w3m-modeline-separator ""
            w3m-modeline-title-string ""))
    (ad-activate 'w3m-modeline-title)

(add-hook 'w3m-display-hook
              (lambda (url)
                (let ((buffer-read-only nil))
                  (delete-trailing-whitespace))))

;; save w3m buffer when save desktop
(defun w3m-register-desktop-save ()
  "Set `desktop-save-buffer' to a function returning the current URL."
  (setq desktop-save-buffer (lambda (desktop-dirname) w3m-current-url)))

(add-hook 'w3m-mode-hook 'w3m-register-desktop-save)

(defun w3m-restore-desktop-buffer (d-b-file-name d-b-name d-b-misc)
  "Restore a `w3m' buffer on `desktop' load."
  (when (eq 'w3m-mode desktop-buffer-major-mode)
    (let ((url d-b-misc))
      (when url
        (require 'w3m)
        (if (string-match "^file" url)
            (w3m-find-file (substring url 7))
          (w3m-goto-url-new-session url))
        (current-buffer)))))

(add-to-list 'desktop-buffer-mode-handlers '(w3m-mode . w3m-restore-desktop-buffer))


;; Dictionary
(defun lookup-word-on-internet (&optional input-word site-to-use)
  "Look up current word or text selection in a online reference site.
  This command launches/switches you to default browser.

  Optional argument INPUT-WORD and SITE-TO-USE can be given.
  SITE-TO-USE a is URL string in this form: 「http://en.wiktionary.org/wiki/�」.
  the 「�」 is a placeholder for the query string.

  If SITE-TO-USE is nil, Google Search is used.

  For a list of online reference sites, see:
  URL `http://ergoemacs.org/emacs/emacs_lookup_ref.html'"
  (interactive)
  (let (ξword refUrl myUrl)
  (setq ξword
  (if input-word
 input-word
 (if (region-active-p)
 (buffer-substring-no-properties (region-beginning) (region-end))
 (thing-at-point 'word) )) )

 (setq refUrl
 (if site-to-use
 site-to-use
 "http://www.google.com/search?q=�" ) )

 (setq myUrl (replace-regexp-in-string "�" ξword refUrl t t))
;;(unless (string= (buffer-name (current-buffer)) "*DICTIONARY*") (other-window 1))
(if (eq (get-buffer "*DICTIONARY*") nil)
(progn
(my-w3m-goto-url-new-session myUrl)
(set-buffer (current-buffer))
(rename-buffer "*DICTIONARY*"))
(progn (switch-to-buffer "*DICTIONARY*") (w3m-goto-url myUrl)))
))
;;(scroll-up-command)
;;(other-window 1)))
;; (browse-url myUrl) ))

 (defun lookup-dictionary (&optional input-word)
 "Lookup current word or text selection in Google Search.
 See also `lookup-word-on-internet'."
 (interactive)
;; (let ((dictUrl "http://oxforddictionaries.com/search?q=�"))
 (let ((dictUrl "http://www.iciba.com/�"))
 (lookup-word-on-internet input-word dictUrl) ) )

(require 'w3m-filter)
(setq w3m-use-filter nil)

(defun my-w3m-filter-rules-for-dictionary (&rest args)
  "Filter rules for Dictionary in w3m."
  (goto-char (point-min))
;;  (while (re-search-forward             ;remove publicize from google.cn or google.com
  ;;        "\\(赞助商链接\\|<h2>Sponsored Links</h2>\\).*aclk.*\\(</cite></ol><p>\\|在此展示您的广告\\)"
    ;;      nil t)
;;    (replace-match ""))
  (while (re-search-forward             ;remove publicize from google.com (English)
          "<nav>"
          nil t)
    (replace-match ""))

  (while (re-search-forward             ;remove publicize from google.com (English)
          "<div id=\"navigation\">"
          nil t)
    (replace-match "")))

 (add-to-list 'w3m-filter-rules
                '("\\`http://oxforddictionaries\\.\\(cn\\|com\\)/"
                  my-w3m-filter-rules-for-dictionary))


(provide 'init-w3m)
