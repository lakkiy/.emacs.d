;;; -*- lexical-binding: t -*-

(use-package newsticker
  :straight (:type built-in)
  :custom
  ;; Keep obsolete items for a month
  (newsticker-keep-obsolete-items t)
  (newsticker-obsolete-item-max-age (* 30 86400))
  ;; Sane behavior
  (newsticker-automatically-mark-items-as-old nil)
  (newsticker-automatically-mark-visited-items-as-old t)
  ;; No logos
  (newsticker-download-logos nil)
  (newsticker-enable-logo-manipulations nil)
  ;; Emacs async sucks
  (newsticker-retrieval-method 'extern)
  ;; Improve the default display
  (newsticker-treeview-listwindow-height 20)
  (newsticker-treeview-date-format "%F %a, %H:%M  ")
  (newsticker-url-list-defaults nil)
  (newsticker-url-list '(("Planet Emacslife" "https://planet.emacslife.com/atom.xml")
                         ("LWN (Linux Weekly News)" "https://lwn.net/headlines/rss"))))

(use-package markdown-mode
  :custom
  (markdown-fontify-code-blocks-natively t)
  :init
  (add-hook 'markdown-mode-hook 'markdown-toggle-markup-hiding))

(provide 'init-reader)
