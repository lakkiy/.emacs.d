;;; -*- lexical-binding: t -*-

(leaf ace-window
  :commands
  (ace-swap-window ace-window)
  :custom-face
  (aw-leading-char-face . '((t (:inherit font-lock-keyword-face :bold t :height 3.0))))
  (aw-minibuffer-leading-char-face . '((t (:inherit font-lock-keyword-face :bold t :height 2.0))))
  (aw-mode-line-face . '((t (:inherit mode-line-emphasis :bold t))))
  :custom
  (aw-keys . '(?a ?o ?e ?u ?i))
  (aw-scope . 'frame))

(provide 'init-window)
