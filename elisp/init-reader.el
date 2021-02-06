;;; -*- lexical-binding: t -*-

(use-package newsticker
  :straight (:type built-in)
  :init
  (setq newsticker-url-list
              '(("Planet Emacslife" "https://planet.emacslife.com/atom.xml")
                ("LWN" "https://lwn.net/headlines/newrss"))))


(provide 'init-reader)
