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


;; Delete the current file

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))


;; Rename the current file

(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))


;; Browse current HTML file

(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))


(eat-package page-break-lines
  :straight t
  :hook (after-init-hook . global-page-break-lines-mode))


(eat-package olivetti
  :straight t
  :commands olivetti-mode)

;; FIXME do not run `xwidget-webkit-browse-url' with the buffer that enable this mode
(define-minor-mode prose-mode
  "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
  :init-value nil :lighter " Prose" :keymap nil
  (if prose-mode
      (progn
        (variable-pitch-mode)
        (auto-fill-mode 1)
        (olivetti-mode)
        (electric-pair-local-mode -1)
        (electric-quote-local-mode)
        (setq-local cursor-type 'bar)
        (setq-local line-spacing 0.2)
        (text-scale-increase 1))
    (variable-pitch-mode -1)
    (auto-fill-mode -1)
    (olivetti-mode -1)
    (electric-pair-local-mode)
    (electric-quote-local-mode -1)
    (kill-local-variable 'cursor-type)
    (kill-local-variable 'line-spacing)
    (text-scale-decrease 1)))


(provide 'init-utils)
