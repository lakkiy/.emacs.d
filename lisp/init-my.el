;;; -*- lexical-binding: t -*-
;; my functions, keys, mode, window...

;; Notifications
;;
;; Actually, `notify-send' is not defined in notifications package, but the
;; autoload cookie will make Emacs load `notifications' first, then our
;; `defalias' will be evaluated.
(pcase system-type
  ('gnu/linux
   (autoload #'notify-send "notifications")
   (with-eval-after-load "notifications"
     (defalias 'notify-send 'notifications-notify)))
  ('darwin
   ;; HACK you must enable notify for emacs in macos system
   ;;      Notifications & Focus -> Emacs -> Allow Notifications
   (defun notify-send (&rest params)
     "Send notifications via `terminal-notifier'."
     (let ((title (plist-get params :title))
           (body (plist-get params :body)))
       (start-process "terminal-notifier"
                      nil
                      "terminal-notifier"
                      "-group" "Emacs"
                      "-title" title
                      "-message" body
                      ;; FIXME this option didn't show emacs icon
                      ;; but -sender didn't show the message when focus on emacs
                      "-activate" "org.gnu.Emacs"))))
  (_
   (defalias 'notify-send 'ignore)))

(defun +reopen-file-with-sudo ()
  (interactive)
  (find-alternate-file (format "/sudo::%s" (buffer-file-name))))
(global-set-key (kbd "C-x C-z") #'+reopen-file-with-sudo)

(defun w/see-you ()
  "Highlight ZERO WIDTH chars in all buffers."
  (interactive)
  (let ((charnames (list "BYTE ORDER MARK"
                         "ZERO WIDTH NO-BREAK SPACE"
                         "ZERO WIDTH SPACE"
                         "RIGHT-TO-LEFT MARK"
                         "RIGHT-TO-LEFT OVERRIDE"
                         "LEFT-TO-RIGHT MARK"
                         "OBJECT REPLACEMENT CHARACTER"

                         "ZERO WIDTH JOINER"
                         "ZERO WIDTH NON-JOINER")))
    (set-face-background 'glyphless-char "RoyalBlue1")
    (dolist (name charnames)
      ;; see info node "info:elisp#Glyphless Chars" for available values
      (set-char-table-range glyphless-char-display
                            (char-from-name name) "fuck"))))

;; This `view-hello-file' always stack me
(global-unset-key (kbd "C-h h"))

;; (require 'url)
;; (require 'json)

(defconst tldr-buffer-name "*tldr*")
(defconst tldr-url-template "https://api.github.com/repos/tldr-pages/tldr/contents/pages/%s/%s.md")

;; Silence compile warnings
(defvar url-http-end-of-headers)

;;;###autoload
(defun tldr (cmd &optional op)
  "View tldr page of CMD.
If OP is non-nil and search failed, OP will be used as platform
name and search again. Typically OP is nil or \"common\"."
  (interactive "sCommand: ")
  (let* ((platform (or op
                       (pcase system-type
                         ('gnu "linux")
                         ('gnu/linux "linux")
                         ('darwin "osx")
                         ('ms-dos "windows"))))
         (url (format tldr-url-template platform cmd)))
    (url-retrieve url
                  (lambda (status)
                    (if (or (not status) (plist-member status :error))
                        (if (not op)
                            (tldr cmd "common")
                          (user-error "Something went wrong.\n\n%s" (pp-to-string (plist-get status :error))))
                      (goto-char url-http-end-of-headers)
                      (let* ((req (json-read))
                             (encoding (alist-get 'encoding req))
                             (content (alist-get 'content req)))
                        (cl-assert (string= encoding "base64"))
                        (let ((buf (get-buffer-create tldr-buffer-name))
                              (inhibit-read-only t))
                          (with-current-buffer buf
                            (erase-buffer)
                            (insert (base64-decode-string content))
                            (when (functionp 'markdown-mode)
                              (markdown-mode))
                            (view-mode +1)
                            (pop-to-buffer buf)))))))))

(provide 'init-my)
