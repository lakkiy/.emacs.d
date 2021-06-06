;;; -*- lexical-binding: t -*-

(straight-use-package 'leetcode)
(straight-use-package 'elfeed)
(straight-use-package 'elfeed-protocol)

(+pdump-packages 'leetcode
                 'elfeed
                 'elfeed-protocol)

;; leetcode
(setq
 leetcode-prefer-language "golang"
 leetcode-prefer-sql "mysql"
 leetcode-save-solutions t
 leetcode-directory "~/Dropbox/leetcode")

(autoload 'leetcode "leetcode" nil t)

;;; elfeed
(setq elfeed-curl-extra-arguments '("--insecure"))

(autoload 'elfeed "elfeed" "Rss reader in emacs." t)

(pretty-hydra-define elfeed-show-hydra (:title "Elfeed Show" :quit-key "q")
  ("Show"
   (("+" elfeed-show-tag "tag")
    ("-" elfeed-show-untag "untag")
    ("n" elfeed-show-next "next")
    ("p" elfeed-show-prev "prev")
    ("y" elfeed-show-yank "yank")
    ("b" elfeed-show-visit "visit")
    ("g" elfeed-show-refresh "refresh")
    ("u" elfeed-show-tag--unread "unread")
    ("P" elfeed-show-play-enclosure "play-enclosure")
    ("d" elfeed-show-save-enclosure "save-enclosure")
    ("s" elfeed-show-new-live-search "new live search")
    ("A" elfeed-show-add-enclosure-to-playlist "add enclosure to playlist")
    ("<tab>" elfeed-show-next-link "next-link"))
   "Kill"
   (("k" elfeed-kill-buffer "buffer" :exit t)
    ("c" elfeed-kill-link-url-at-point "point url link" :exit t))))

(pretty-hydra-define elfeed-search-hydra (:title "Elfeed Search" :quit-key "q")
  ("Search"
   (("y" elfeed-search-yank "yank")
    ("G" elfeed-search-fetch "fetch")
    ("g" elfeed-search-update--force "force update")
    ("+" elfeed-search-tag-all "tag")
    ("-" elfeed-search-untag-all "untag")
    ("b" elfeed-search-browse-url "browse")
    (">" elfeed-search-last-entry "last")
    ("<" elfeed-search-first-entry "first")
    ("S" elfeed-search-set-filter "filter")
    ("s" elfeed-search-live-filter "live filter")
    ("Q" elfeed-search-quit-window "quit" :exit t)
    ("c" elfeed-search-clear-filter "clear filter")
    ("u" elfeed-search-tag-all-unread "tag all unread")
    ("r" elfeed-search-untag-all-unread "untag all unread")
    ("<tab>" elfeed-search-show-entry "show" :exit t))))

(with-eval-after-load "elfeed"
  (define-key elfeed-search-mode-map (kbd "C-c C-h") 'elfeed-search-hydra/body)
  (define-key elfeed-show-mode-map (kbd "C-c C-h") 'elfeed-show-hydra/body)
  (elfeed-set-timeout 36000)
  ;; elfeed-protocol
  (elfeed-protocol-enable))

(provide 'init-mole)
