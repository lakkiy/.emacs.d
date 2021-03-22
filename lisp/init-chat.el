;; -*- lexical-binding: t; -*-

(leaf telega
  :straight
  (telega :type git
          :host github
          :branch "releases")
  :commands
  (telega))

(provide 'init-chat)
