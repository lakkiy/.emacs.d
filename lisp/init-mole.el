;;; -*- lexical-binding: t -*-

(straight-use-package 'leetcode)
;; (straight-use-package 'elfeed)
;; (straight-use-package 'elfeed-protocol)

(+pdump-packages 'leetcode)

;; leetcode
(setq
 leetcode-prefer-language "golang"
 leetcode-prefer-sql "mysql"
 leetcode-save-solutions t
 leetcode-directory "~/Dropbox/leetcode")

(autoload 'leetcode "leetcode" nil t)

;; elfeed
;; (setq elfeed-curl-extra-arguments '("--insecure"))
;;
;; (autoload 'elfeed "elfeed" nil t)
;;
;; (with-eval-after-load "elfeed"
;;   (elfeed-set-timeout 36000)
;;   ;; elfeed-protocol
;;   (elfeed-protocol-enable))

(provide 'init-mole)
