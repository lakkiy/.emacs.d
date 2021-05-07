;;; -*- lexical-binding: t -*-

;; TODO elfeed
(straight-use-package 'markdown-mode)
(straight-use-package 'go-translate)

;; markdown-mode
(setq markdown-fontify-code-blocks-natively t)
(add-hook 'markdown-mode-hook 'markdown-toggle-markup-hidding)

;; go-translate
(setq go-translate-base-url "https://translate.google.cn")
(setq go-translate-local-language "zh-CN")
(setq go-translate-inputs-function #'go-translate-inputs-current-or-prompt)
(setq go-translate-token-current (cons 430675 2721866130))

(provide 'init-reader)
