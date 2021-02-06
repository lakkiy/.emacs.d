;;; -*- lexical-binding: t -*-

(use-package valign
  :straight
  (valign :type git
          :host github
          :repo "casouri/valign")
  :config
  (add-hook 'org-mode-hook #'valign-mode))

(use-package org
  :straight (:type built-in))

(provide 'init-org)
