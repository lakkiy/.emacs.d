;;; -*- lexical-binding: t -*-

(setq python-indent-offset 4
      python-shell-completion-native-enable nil
      python-indent-guess-indent-offset nil)

(install-package 'pet)
;; This will turn on `pet-mode' on `python-mode' and `python-ts-mode'
(add-hook 'python-base-mode-hook 'pet-mode -10)

(provide 'init-python)
