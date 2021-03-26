;;; -*- lexical-binding: t -*-

(leaf go-mode
  :straight t
  :hook (before-save-hook . gofmt-before-save)
  :mode "\\.go\\'"
  :pre-setq
  (gofmt-command . "goimports")
  :config
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY"))))

(leaf flymake-golangci
  :straight
  (flymake-golangci :type git :host gitlab :repo "shackra/flymake-golangci")
  :after go-mode
  :doc "best golang lint tool"
  :hook (go-mode-hook . flymake-golangci-load))

(leaf gotest
  :straight t
  :after go-mode
  :doc "run go test in emacs")

(leaf go-gen-test
  :straight t
  :after go-mode
  :doc "generate golang test"
  :commands
  (go-gen-test-dwim))

(leaf go-tag
  :straight t
  :after go-mode
  :doc "generate golang struct tag"
  :init
  (setq go-tag-args (list "-transform" "camelcase"))
  :commands
  (go-tag-add go-tag-remove go-tag-refresh))

(leaf go-dlv
  :straight t
  :after go-mode
  :added "2021-03-17"
  :doc "debug golang program"
  :commands
  (dlv dlv-current-fun))

(leaf go-fill-struct
  :straight t
  :added "2021-03-17"
  :after go-mode
  :commands go-fill-struct)

(leaf go-impl
  :straight t
  :added "2021-03-17"
  :after go-mode
  :commands go-impl)

(provide 'init-go)
