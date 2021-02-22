;;; -*- lexical-binding: t -*-

(leaf python
  :tag "builtin"
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil))

(leaf highlight-indentation
  :straight t
  :hook
  (python-mode-hook . highlight-indentation-mode))

(provide 'init-python)
