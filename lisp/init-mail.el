;;; -*- lexical-binding: t -*-

(defface notmuch-tag-emacs
  '((t :foreground "systemPurpleColor"))
  "Default face used for the emacs tag.

Used in the default value of `notmuch-tag-formats'."
  :group 'notmuch-faces)

(defface notmuch-tag-golang
  '((t :foreground "systemBlueColor"))
  "Default face used for the golang tag.

Used in the default value of `notmuch-tag-formats'."
  :group 'notmuch-faces)

(eat-package notmuch
  :straight t
  :commands notmuch
  :init
  (setq notmuch-show-logo nil
        notmuch-search-oldest-first nil
        notmuch-search-result-format '(("date" . "%12s ")
                                       ("count" . "%-11s ")
                                       ("authors" . "%-20s ")
                                       ("subject" . "%s ")
                                       ("tags" . "(%s)"))
        notmuch-show-empty-searches t)
  :config
  (add-to-list 'notmuch-tag-formats '("emacs" (propertize tag 'face 'notmuch-tag-emacs)))
  (add-to-list 'notmuch-tag-formats '("golang" (propertize tag 'face 'notmuch-tag-golang))))

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
