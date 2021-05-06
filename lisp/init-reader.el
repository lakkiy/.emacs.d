;;; -*- lexical-binding: t -*-

;; TODO elfeed
(straight-use-package 'markdown-mode)
(straight-use-package '(multi-translate :type git :host github :repo "twlz0ne/multi-translate.el"))

;; markdown-mode
(setq markdown-fontify-code-blocks-natively t)
(add-hook 'markdown-mode-hook 'markdown-toggle-markup-hidding)

;; multi-translate

(provide 'init-reader)
