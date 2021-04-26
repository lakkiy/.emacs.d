;;; -*- lexical-binding: t -*-

(leaf joker-theme
  :straight
  (joker-theme :type git
               :host github
               :repo "DogLooksGood/joker-theme")
  :setq
  (joker-theme-main-color . "#B762DE")
  :require t)
(leaf modus-themes
  :straight
  (modus-themes :type git
                :host gitlab
                :repo "protesilaos/modus-themes"))

(leaf nasy-theme
  :straight
  (nasy-theme :type git
              :host github
              :repo "404cn/nasy-theme.el"))
(leaf lazycat-theme
  :straight
  (lazycat-theme :type git
                 :host github
                 :repo "404cn/lazycat-theme"))

(load-theme 'lazycat-dark t)

(provide 'init-themes)
