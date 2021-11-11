;;; -*- lexical-binding: t -*-

;; icon
(setq +foo-p nil)

;; modeline
(setq +doom-modeline-p nil)


;; theme
(setq +theme nil)
(setq +theme-tui nil)
(setq +theme-system-appearance nil)
(setq +theme-system-light nil)
(setq +theme-system-dark nil)


;; elfeed
(setq elfeed-feeds (list
                    (list "fever+https://user@myhost.com"
                          :api-url "https://myhost.com/plugins/fever/"
                          :password "password/with|special@characters:"
                          :autotags '(("example.com" comic)))))


;; telega
(setq telega-proxies nil)
