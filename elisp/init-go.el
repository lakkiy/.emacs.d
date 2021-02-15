;;; -*- lexical-binding: t -*-

;; (use-package go-mode
;;   :hook (before-save . gofmt-before-save)
;;   :init
;;   (setq gofmt-command "goimports")
;;   :config
;;   (with-eval-after-load 'exec-path-from-shell
;;     (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))
;;
;;   (use-package flymake-golangci
;;     :straight
;;     (flymake-golangci :type git
;;                       :host gitlab
;;                       :repo "shackra/flymake-golangci")
;;     :hook
;;     (go-mode . flymake-golangci-load))
;;
;;   (use-package gotest)
;;   (use-package go-gen-test)
;;   (use-package go-tag))

(leaf go-mode
  :straight t
  :hook (before-save-hook . gofmt-before-save)
  :pre-setq
  (gofmt-command . "goimports")
  :config
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH"))))

(leaf flymake-golangci
  :straight
  (flymake-golangci :type git :host gitlab :repo "shackra/flymake-golangci")
  :after go-mode
  :hook (go-mode-hook . flymake-golangci-load))

(leaf gotest :straight t :after go-mode)
(leaf go-gen-test :straight t :after go-mode)
(leaf go-tag :straight t :after go-mode)

(provide 'init-go)
