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

(provide 'init-mole)
