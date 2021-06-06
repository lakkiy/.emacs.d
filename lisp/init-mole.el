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
(setq
 elfeed-search-filter "+unread "
 elfeed-curl-extra-arguments '("--insecure")
 browse-url-browser-function 'eww-browse-url)

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

(defun +elfeed-search-star-tag-all ()
  (interactive)
  (elfeed-search-tag-all 'star))

(defun +elfeed-search-star-untag-all ()
  (interactive)
  (elfeed-search-untag-all 'star))

(pretty-hydra-define elfeed-search-hydra (:title "Elfeed Search" :quit-key "q")
  ("Search"
   (("y" elfeed-search-yank "yank")
    ("G" elfeed-search-fetch "fetch")
    ("g" elfeed-search-update--force "force update")
    ("+" elfeed-search-tag-all "tag")
    ("-" elfeed-search-untag-all "untag")
    ("t" +elfeed-search-star-tag-all "star")
    ("T" +elfeed-search-star-untag-all "unstar")
    ("b" elfeed-search-browse-url "browse" :exit t)
    (">" elfeed-search-last-entry "last")
    ("<" elfeed-search-first-entry "first")
    ("S" elfeed-search-set-filter "filter")
    ("s" elfeed-search-live-filter "live filter")
    ("Q" elfeed-search-quit-window "quit" :exit t)
    ("c" elfeed-search-clear-filter "clear filter")
    ("u" elfeed-search-tag-all-unread "unread")
    ("r" elfeed-search-untag-all-unread "read")
    ("<tab>" elfeed-search-show-entry "show" :exit t))))

(with-eval-after-load "elfeed"
  (advice-add 'elfeed-search-browse-url
              :before
              #'elfeed-search-untag-all-unread)
  (add-hook 'eww-mode-hook 'visual-line-mode)
  (define-key elfeed-search-mode-map (kbd "C-c C-h") 'elfeed-search-hydra/body)
  (define-key elfeed-search-mode-map (kbd "t") '+elfeed-search-star-tag-all)
  (define-key elfeed-search-mode-map (kbd "T") '+elfeed-search-star-untag-all)
  (define-key elfeed-show-mode-map (kbd "C-c C-h") 'elfeed-show-hydra/body)
  (elfeed-set-timeout 36000)
  ;; elfeed-protocol
  (elfeed-protocol-enable))

(provide 'init-mole)
