;;; -*- lexical-binding: t -*-

(eat-package js2-mode
  :straight t
  :mode
  ("\\.js\\'" . js2-mode)
  ("\\.jsx\\'" . js2-jsx-mode))

(eat-package typescript-mode
  :mode ("\\.ts[x]\\'" . typescript-mode))

(provide 'init-web)
