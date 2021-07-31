;;; -*- lexical-binding: t -*-

(setq +font "Monaco")
(setq +font-cn "WenQuanYi Micro Hei")
(setq +font-unicode "Apple Color Emoji")
(setq +font-variable-pitch "Bookerly")
(setq +font-height 130)
(setq +use-header-line nil)
(setq +theme 'minidark)
(setq +theme-tui 'minidark)
(setq +theme-system-light 'doom-solarized-light)
(setq +theme-system-dark 'doom-solarized-dark)
(setq +enable-proxy? nil)
(setq +proxy "127.0.0.1:7890")
(setq +erc-password "")

(setq elfeed-feeds (list
                    (list "fever+https://user@myhost.com"
                          :api-url "https://myhost.com/plugins/fever/"
                          :password "password/with|special@characters:"
                          :autotags '(("example.com" comic)))))
