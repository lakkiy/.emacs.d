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

;;; Nice window divider
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?┃))

;;; Transparency
(let ((alpha 100))
  (add-to-list 'default-frame-alist (cons 'alpha alpha)))

(leaf circadian
  :straight
  (circadian :type git
             :host github
             :repo "guidoschmidt/circadian.el")
  :require
  (joker-theme printed-theme storybook-theme)
  :config
  ;; Beijing
  (setq calendar-latitude 39.904202)
  (setq calendar-longitude 116.407394)
  (setq circadian-themes '((:sunrise . storybook)
                           (:sunset  . joker)))
  (circadian-setup))

(provide 'init-themes)
