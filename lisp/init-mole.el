;;; -*- lexical-binding: t -*-

(eat-package elfeed
  :straight t
  :commands elfeed
  :init
  (setq elfeed-search-filter "+unread "
        ;; browse-url-browser-function 'eww-browse-url
        elfeed-curl-extra-arguments '("--insecure"))

  :config
  (elfeed-set-timeout 36000))

(eat-package elfeed-protocol
  :straight t
  :after elfeed
  :config
  (elfeed-protocol-enable))

(eat-package leetcode
  :straight (leetcode :type git :host github :repo "ginqi7/leetcode-emacs")
  :commands leetcode-list-all
  :init
  (eat-package ctable :straight t)
  (setq leetcode-language "go"))

(provide 'init-mole)
