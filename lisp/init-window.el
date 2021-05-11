;;; -*- lexical-binding: t -*-

(straight-use-package 'ace-window)

;; ace-window
(setq
 aw-keys '(?a ?o ?e ?u ?i)
 aw-scope 'frame)

(autoload 'ace-swap-window "ace-window")
(autoload 'ace-window "ace-window")

(custom-set-faces
 '(aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 2.0)))))

(provide 'init-window)
