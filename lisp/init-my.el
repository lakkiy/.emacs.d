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

(provide 'init-my)
