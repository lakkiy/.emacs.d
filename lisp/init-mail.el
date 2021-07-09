;;; -*- lexical-binding: t -*-

(eat-package mbsync
  :straight t)

(eat-package notmuch
  :straight t
  :commands notmuch
  :init
  ;; TODO gpg encrypt gmail app password in ~/.authinfo
  (setq user-full-name "Liu Bo"
        user-mail-address "liubolovelife@gmail.com"
        send-mail-function #'smtpmail-send-it
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smptmail-stream-type 'ssl
        smtpmail-smtp-user "liubolovelife@gmail.com")
  (setq notmuch-show-logo nil
        notmuch-search-oldest-first nil
        notmuch-search-result-format '(("date" . "%12s ")
                                       ("count" . "%-11s ")
                                       ("authors" . "%-20s ")
                                       ("subject" . "%s ")
                                       ("tags" . "(%s)"))
        notmuch-show-empty-searches t))

(provide 'init-mail)
