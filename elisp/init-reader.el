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
                         ("Mastering Emacs" "http://www.masteringemacs.org/feed/")
                         ("Oremacs" "https://oremacs.com/atom.xml")
                         ("EmacsCast" "https://pinecast.com/feed/emacscast")
                         ("LWN" "https://lwn.net/headlines/rss"))))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-fontify-code-blocks-natively t)
  :init
  (add-hook 'markdown-mode-hook 'markdown-toggle-markup-hiding))

(use-package go-translate
  :straight
  (go-translate :type git
                :host github
                :repo "lorniu/go-translate")
  :bind
  ("C-c t" . go-translate)
  :init
  (setq go-translate-token-current (cons 430675 2721866130))
  (setq go-translate-inputs-function #'go-translate-inputs-current-or-prompt)
  (setq go-translate-base-url "https://translate.google.cn")
  (setq go-translate-local-language "zh-CN"))

(provide 'init-reader)
