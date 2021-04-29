;;; -*- lexical-binding: t -*-

(straight-use-package 'yascroll)

(add-hook 'prog-mode-hook 'yascroll-bar-mode)
(add-hook 'conf-mode-hook 'yascroll-bar-mode)



(straight-use-package 'ace-window)

(setq
 aw-keys '(?a ?o ?e ?u ?i)
 aw-scope 'frame)

(autoload 'ace-swap-window "ace-window")
(autoload 'ace-window "ace-window")

;; TODO customize face

(provide 'init-window)
