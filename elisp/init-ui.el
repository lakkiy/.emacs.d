;;; -*- lexical-binding: t -*-

(require 'init-themes)
(require 'init-utils)

(defun +simple-mode-line-render (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
               (string-width (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

(setq-default mode-line-format
              '((:eval
                 (+simple-mode-line-render
                  ;; left
                  '((:eval (meow-indicator))
                    " %l:%C "
                    (:propertize (-3 "%p") face +modeline-dim-face))
                  ;; right
                  '((:eval (rime-lighter))
                    " "
                    (:propertize mode-name face font-lock-keyword-face)
                    " "
                    (:eval (+smart-file-name-with-propertize))
                    " ")))))

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

(provide 'init-ui)
