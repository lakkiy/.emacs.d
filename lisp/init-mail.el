;;; -*- lexical-binding: t -*-

(straight-use-package 'notmuch)

(+pdump-packages 'notmuch)

;;; notmuch TODO
;; add this to mbsyncrc on macos to fix AuthMechs PLAIN
(setq
 notmuch-show-logo nil
 notmuch-search-oldest-first nil
 notmuch-search-result-format '(("date" . "%12s ")
                                ("count" . "%-11s ")
                                ("authors" . "%-20s ")
                                ("subject" . "%s ")
                                ("tags" . "(%s)"))
 notmuch-show-empty-searches t)

(autoload 'notmuch "notmuch" "notmuch mail" t)

(defun +notmuch-toggle-deleted-tag()
  "toggle deleted tag for message"
  (interactive)
  (if (member "deleted" (notmuch-show-get-tags))
      (notmuch-show-tag (list "-deleted"))
    (notmuch-show-tag (list "+deleted"))))

(with-eval-after-load "notmuch"
  (define-key notmuch-search-mode-map "d" #'+notmuch-toggle-deleted-tag))

;;; sendmail and smtpmail
;; TODO gpg encrypt gmail app password in ~/.authinfo
(setq
 user-full-name "Liu Bo"
 user-mail-address "liubolovelife@gmail.com"
 send-mail-function #'smtpmail-send-it
 smtpmail-default-smtp-server "smtp.gmail.com"
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-smtp-service 587
 smptmail-stream-type 'ssl
 smtpmail-smtp-user "liubolovelife@gmail.com")

;; TODO add hydra to add header in message-mode
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Mail-Headers.html

(provide 'init-mail)
