;;; -*- lexical-binding: t -*-

(straight-use-package 'notmuch)

(+pdump-packages 'notmuch)

;;; notmuch TODO
(setq
 notmuch-show-logo nil)

(autoload 'notmuch "notmuch" "notmuch mail" t)

(provide 'init-mail)
