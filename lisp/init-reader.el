;;; -*- lexical-binding: t -*-

;; (straight-use-package 'elfeed)
;; (leaf elfeed
;;   :doc "rss reader interface for emacs"
;;   :added "2021-03-12"
;;   :commands
;;   (elfeed))
;;
;; (straight-use-package 'elfeed-protocol)
;; (leaf elfeed-protocol
;;   :doc "Provide extra protocols to make self-hosting RSS readers work with elfeed"
;;   :added "2021-03-12"
;;   :after elfeed
;;   :init
;;   ;; TODO
;;   (setq elfeed-feeds nil)
;;   :config
;;   (elfeed-protocol-enable))

(leaf hackernews
  :doc "can't open hackernews in company's network"
  :straight
  (hackernews :type git
              :host github
              :repo "clarete/hackernews.el")
  :commands
  (hackernews))

(leaf markdown-mode
  :straight t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-fontify-code-blocks-natively . t)
  :init
  (add-hook 'markdown-mode-hook 'markdown-toggle-markup-hiding))

(leaf go-translate
  :straight
  (go-translate :type git :host github :repo "lorniu/go-translate")
  :bind (("C-c t" . go-translate))
  :commands
  (go-translate go-translate-popup)
  :init
  (setq go-translate-token-current (cons 430675 2721866130))
  (setq go-translate-inputs-function #'go-translate-inputs-current-or-prompt)
  (setq go-translate-base-url "https://translate.google.cn")
  (setq go-translate-local-language "zh-CN"))

(provide 'init-reader)
