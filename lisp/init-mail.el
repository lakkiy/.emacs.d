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

(provide 'init-mail)
