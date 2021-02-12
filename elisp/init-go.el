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

  (use-package go-tag
    :bind (:map go-mode-map
           ("C-c t t" . go-tag-add)
           ("C-c t T" . go-tag-remove))
    :init (setq go-tag-args (list "-transform" "camelcase"))))

(provide 'init-go)
