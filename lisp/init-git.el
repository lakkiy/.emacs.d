;;; -*- lexical-binding: t -*-

(leaf magit
  :straight t
  :commands magit)

(leaf gitmoji
  :straight
  (gitmoji :type git
           :host github
           :repo "Tiv0w/gitmoji")
  :after magit
  :init
  (setq gitmoji--insert-utf8-emoji t
        gitmoji--display-utf8-emoji t))

(provide 'init-git)
