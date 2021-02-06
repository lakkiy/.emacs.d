;;; -*- lexical-binding: t -*-

(use-package joker-theme
  :straight
  (joker-theme :type git
               :host github
               :repo "DogLooksGood/joker-theme"))

(use-package printed-theme
  :straight
  (printed-theme :type git
                 :host github
                 :repo "DogLooksGood/printed-theme"))

(use-package storybook-theme
  :straight
  (storybook-theme :type git
                   :host github
                   :repo "DoglooksGood/storybook-theme"))

(provide 'init-themes)
