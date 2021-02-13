;;; -*- lexical-binding: t -*-

(use-package go-mode
  :hook (before-save . gofmt-before-save)
  :init
  (setq gofmt-command "goimports")
  :config
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))

  (use-package flymake-golangci
    :straight
    (flymake-golangci :type git
                      :host gitlab
                      :repo "shackra/flymake-golangci")
    :hook
    (go-mode . flymake-golangci-load))

  (use-package gotest)
  (use-package go-gen-test)
  (use-package go-tag)

(provide 'init-go)
