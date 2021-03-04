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

(leaf modus-themes
  :straight
  (modus-themes :type git
                :host gitlab
                :repo "protesilaos/modus-themes"))

(leaf atom-one-dark-theme :straight t)
(leaf spacemacs-theme :straight t)
(leaf cyberpunk-theme :straight t)
(leaf solarized-theme :straight t)
(leaf dracula-theme :straight t)
(leaf minimal-theme :straight t)
(leaf nord-theme :straight t)
(leaf tao-theme :straight t)
;; TODO split citylights theme out of doom themes
(leaf nasy-theme)

;;; Nice window divider
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?┃))

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
                           (:sunset  . modus-vivendi)))
  (circadian-setup))

(provide 'init-themes)
