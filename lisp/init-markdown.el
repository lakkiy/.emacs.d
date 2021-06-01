;;; -*- lexical-binding: t -*-
(straight-use-package 'markdown-mode)

;; markdown-mode
(setq markdown-fontify-code-blocks-natively t)
(add-hook 'markdown-mode-hook 'markdown-toggle-markup-hidding)

(provide 'init-markdown)
