;;; -*- lexical-binding: t -*-

(leaf joker-theme
  :straight
  (joker-theme :type git
               :host github
               :repo "DogLooksGood/joker-theme"))

(leaf printed-theme
  :straight
  (printed-theme :type git
                 :host github
                 :repo "DogLooksGood/printed-theme"))

(leaf storybook-theme
  :straight
  (storybook-theme :type git
                   :host github
                   :repo "DoglooksGood/storybook-theme"))

(provide 'init-themes)
