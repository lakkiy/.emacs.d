;;; -*- lexical-binding: t -*-

(eat-package mbsync
  :straight t)

(defface notmuch-search-emacs-face
  '((((class color)
      (background dark))
     (:foreground "White" :background "systemPurpleColor"))
    (((class color)
      (background light))
     (:foreground "White" :background "systemPurpleColor")))
  "Face used in search mode face.

This face is the default value for the \"emacs\" tag in
`notmuch-search-line-faces'."
  :group 'notmuch-search
  :group 'notmuch-faces)

(defface notmuch-search-golang-face
  '((((class color)
      (background dark))
     (:foreground "White" :background "systemBlueColor"))
    (((class color)
      (background light))
     (:foreground "White" :background "systemBlueColor")))
  "Face used in search mode face.

This face is the default value for the \"golang\" tag in
`notmuch-search-line-faces'."
  :group 'notmuch-search
  :group 'notmuch-faces)

(eat-package notmuch
  :straight t
  :commands notmuch
  :init
  (add-to-list 'notmuch-search-line-faces '("emacs" . notmuch-search-emacs-face))
  (add-to-list 'notmuch-search-line-faces '("golang" . notmuch-search-golang-face))
  (setq notmuch-show-logo nil
        notmuch-search-oldest-first nil
        notmuch-search-result-format '(("date" . "%12s ")
                                       ("count" . "%-11s ")
                                       ("authors" . "%-20s ")
                                       ("subject" . "%s ")
                                       ("tags" . "(%s)"))
        notmuch-show-empty-searches t))

(eat-package message
  :hook (message-mode-hook . auto-fill-mode)
  :init
  (setq user-full-name "Liu Bo"
        user-mail-address "liubolovelife@gmail.com"
        message-kill-buffer-on-exit t
        message-mail-alias-type 'ecomplete
        message-send-mail-function #'message-use-send-mail-function
        message-signature user-full-name))

(eat-package sendmail
  :init
  (setq send-mail-function #'smtpmail-send-it))

(eat-package smtpmail
  :init
  (setq smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-user user-mail-address
        smtpmail-smtp-service 587
        smptmail-stream-type 'ssl))

(provide 'init-mail)
