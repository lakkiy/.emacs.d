;;; -*- lexical-binding: t -*-

(leaf python
  :tag "builtin"
  :mode "\\.py\\'"
  :hook
  (python-mode-hook . flymake-mode)
  (python-mode-hook . blacken-mode)
  :custom
  (python-indent-offset . 4)
  (python-shell-interpreter . "python3")
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil))

(leaf blacken
  :straight t
  :doc "used to format python buffers"
  :added "2021-03-09"
  :after python
  :init
  (setq-default blacken-fast-unsafe t)
  (setq-default blacken-line-length 80))

 (provide 'init-python)
