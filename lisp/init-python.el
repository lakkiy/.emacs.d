;;; -*- lexical-binding: t -*-

(setq
 python-indent-offset 4
 python-shell-completion-native-enable nil
 python-shell-interpreter "python3"
 python-indent-guess-indent-offset nil)

(with-eval-after-load "python"
  (add-hook 'python-mode-hook 'eglot-ensure))

(provide 'init-python)
