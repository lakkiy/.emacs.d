;;; -*- lexical-binding: t -*-

(straight-use-package 'go-mode)

(add-hook 'before-save-hook 'gofmt-before-save)

(setq gofmt-command "goimports")

(with-eval-after-load "exec-path-from-shell"
  (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))



(straight-use-package '(flymake-golangci :type git :host gitlab :repo "shackra/flymake-golangci"))

(add-hook 'go-mode-hook 'flymake-golangci-load)


(straight-use-package 'gotest)
(straight-use-package 'go-gen-test)
(straight-use-package 'go-dlv)
(straight-use-package 'go-fill-struct)
(straight-use-package 'go-impl)



(straight-use-package 'go-tag)

(setq go-tag-args (list "-transform" "camelcase"))



(provide 'init-go)
