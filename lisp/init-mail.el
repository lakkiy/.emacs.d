;;; -*- lexical-binding: t -*-

(straight-use-package 'notmuch)

(+pdump-packages 'notmuch)

;;; notmuch TODO
;; add this to mbsyncrc on macos to fix AuthMechs PLAIN
(setq
 notmuch-show-logo nil)

(autoload 'notmuch "notmuch" "notmuch mail" t)

(provide 'init-mail)
