;;; -*- lexical-binding: t -*-

(use-package go-translate
  :straight
  (go-translate :type git
                :host github
                :repo "lorniu/go-translate")
  :bind
  ("C-c t" . go-translate)
  :init
  (setq go-translate-token-current (cons 430675 2721866130))
  (setq go-translate-inputs-function #'go-translate-inputs-current-or-prompt)
  (setq go-translate-base-url "https://translate.google.cn")
  (setq go-translate-local-language "zh-CN"))

(use-package telega
  :bind
  ("<f6>" . telega))

(use-package docker
  :commands (docker))

(use-package docker-compose-mode)

(use-package dockerfile-mode)

(use-package hackernews
  :straight
  (hackernews :type git
              :host github
              :repo "clarete/hackernews.el"))

(provide 'init-fun)
