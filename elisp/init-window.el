;;; -*- lexical-binding: t -*-

(use-package ace-window
  :bind
  ("C-x o" . ace-window)
  :commands
  (ace-swap-window ace-window)
  :custom
  (aw-keys '(?a ?o ?e ?u ?i))
  (aw-scope 'frame))

(provide 'init-window)
