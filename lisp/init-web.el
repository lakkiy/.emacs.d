;;; -*- lexical-binding: t -*-

(setq-default
 js-indent-level 2
 css-indent-offset 2)

(eat-package typescript-mode
  :straight t
  :mode ("\\.ts[x]\\'" . typescript-mode))

(provide 'init-web)
