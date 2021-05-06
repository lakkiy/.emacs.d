;;; -*- lexical-binding: t -*-

;; TODO elfeed
(straight-use-package 'markdown-mode)
(straight-use-package '(go-translate :type git :host github :repo "lorniu/go-translate"))

;; markdown-mode
(setq markdown-fontify-code-blocks-natively t)
(add-hook 'markdown-mode-hook 'markdown-toggle-markup-hidding)

;; go-translate
(setq
 go-translate-token-current (cons 430675 2721866130)
 go-translate-inputs-function #'go-translate-inputes-current-or-prompt
 go-translate-base-url "https://translate.google.cn"
 go-translate-local-language "zh-CN")

(autoload 'go-translate "go-translate" nil t)
(autoload 'go-translate-popup "go-translate" nil t)

(provide 'init-reader)
