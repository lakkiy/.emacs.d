;;; init-tramp.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Tramp
(setq tramp-verbose 0)

;; Set remote-file-name-inhibit-cache to nil if remote files are not
;; independently updated outside TRAMP’s control. That cache cleanup
;; will be necessary if the remote directories or files are updated
;; independent of TRAMP.
(setq remote-file-name-inhibit-cache nil)

;;  Disable file locks. Set remote-file-name-inhibit-locks to t if
;;  you know that different Emacs sessions are not modifying the same
;;  remote file.
(setq remote-file-name-inhibit-locks t)

;; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
(setq tramp-use-scp-direct-remote-copying t
      tramp-copy-size-limit 1000000
      remote-file-name-inhibit-auto-save-visited t)

(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)

(with-eval-after-load 'tramp
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook
                 #'tramp-compile-disable-ssh-controlmaster-options)))

(defun sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (when (file-writable-p file)
    (user-error "File is user writeable, aborting sudo"))
  (find-file (if (file-remote-p file)
		         (concat "/" (file-remote-p file 'method) ":"
			             (file-remote-p file 'user) "@" (file-remote-p file 'host)
			             "|sudo:root@"
			             (file-remote-p file 'host) ":" (file-remote-p file 'localname))
	           (concat "/sudo:root@localhost:" file))))

(defun sudo-this-file ()
  "Open the current file as root."
  (interactive)
  (sudo-find-file (file-truename buffer-file-name)))
(keymap-global-set "C-x C-z" #'sudo-this-file)

(with-eval-after-load 'tramp
  ;; ‘Private Directories’ are the settings of the $PATH environment,
  ;; as given in your ‘~/.profile’.  This entry is represented in
  ;; the list by the special value ‘tramp-own-remote-path’.
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;;; init-tramp.el ends here
