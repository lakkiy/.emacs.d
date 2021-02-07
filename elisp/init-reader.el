;;; -*- lexical-binding: t -*-

(use-package newsticker
  :straight (:type built-in)
  :init
  (setq newsticker-url-list
              '(("Planet Emacslife" "https://planet.emacslife.com/atom.xml")
                ("LWN" "https://lwn.net/headlines/newrss"))))

(use-package markdown-mode
  :custom
  (markdown-fontify-code-blocks-natively t)
  :init
  (add-hook 'markdown-mode-hook 'markdown-toggle-markup-hiding))

(provide 'init-reader)
