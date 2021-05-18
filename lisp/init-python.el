;;; -*- lexical-binding: t -*-

(setq python-indent-offset 4
      python-shell-completion-native-enable nil)

(with-eval-after-load "python"
  (add-hook 'python-mode-hook 'eglot-ensure))

(provide 'init-python)
