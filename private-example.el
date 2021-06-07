;;; -*- lexical-binding: t -*-

(setq
 +font "PxPlus IBM VGA8"
 +font-cn "Unifont"
 +font-unicode "Unifont"
 +font-height 130
 +use-header-line nil
 +theme 'minidark
 +enable-proxy? nil
 +proxy "127.0.0.1:7890"
 +erc-password ""
 ;; for fever
 elfeed-feeds (list
                    (list "fever+https://user@myhost.com"
                          :api-url "https://myhost.com/plugins/fever/"
                          :password "password/with|special@characters:"
                          :autotags '(("example.com" comic)))))
