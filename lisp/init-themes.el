;;; -*- lexical-binding: t -*-

(leaf joker-theme
  :straight
  (joker-theme :type git
               :host github
               :repo "DogLooksGood/joker-theme")
  :setq
  (joker-theme-main-color . "#B762DE")
  :require t)
(leaf printed-theme
  :straight
  (printed-theme :type git
                 :host github
                 :repo "DogLooksGood/printed-theme")
  :require t)
(leaf storybook-theme
  :straight
  (storybook-theme :type git
                   :host github
                   :repo "DoglooksGood/storybook-theme")
  :require t)

(leaf lazycat-theme
  :straight
  (lazycat-theme :type git
                 :host github
                 :repo "404cn/lazycat-theme"))

(leaf acme-theme :straight t :setq (acme-theme-black-fg . t))
(leaf nimbus-theme :straight t)
(leaf atom-one-dark-theme :straight t)
(leaf spacemacs-theme :straight t)
(leaf solarized-theme :straight t)
(leaf dracula-theme :straight t)
(leaf modus-themes
  :straight
  (modus-themes :type git
                :host gitlab
                :repo "protesilaos/modus-themes"))

;; for terminal
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?┃))

(leaf nasy-theme)

(leaf circadian
  :straight
  (circadian :type git
             :host github
             :repo "guidoschmidt/circadian.el")
  :config
  ;; Beijing
  (setq calendar-latitude 39.904202)
  (setq calendar-longitude 116.407394)
  (setq circadian-themes '((:sunrise . nasy)
                           (:sunset  . joker)))
  (circadian-setup))

(provide 'init-themes)
