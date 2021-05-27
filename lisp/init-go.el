;;; -*- lexical-binding: t -*-

(straight-use-package 'go-mode)
(straight-use-package 'gotest)
(straight-use-package 'go-gen-test)
(straight-use-package 'go-dlv)
(straight-use-package 'go-fill-struct)
(straight-use-package 'go-impl)
(straight-use-package 'go-tag)
(straight-use-package '(flymake-golangci :type git :host gitlab :repo "shackra/flymake-golangci"))

;;; go-mode
(setq gofmt-command "goimports")

(with-eval-after-load "go-mode"
  (with-eval-after-load "exec-path-from-shell"
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'before-save-hook 'gofmt-before-save)

  (add-hook 'go-test-mode-hook 'visual-line-mode)

  ;;; flymake-golangci
  (add-hook 'go-mode-hook 'flymake-golangci-load)

  (define-key go-mode-map (kbd "C-c t g") #'go-gen-test-dwim)
  (define-key go-mode-map (kbd "C-c t m") #'go-test-current-file)
  (define-key go-mode-map (kbd "C-c t .") #'go-test-current-test)
  (define-key go-mode-map (kbd "C-c t t") #'go-tag-add)
  (define-key go-mode-map (kbd "C-c t T") #'go-tag-remove)
  (define-key go-mode-map (kbd "C-c t x") #'go-run)

  (with-eval-after-load "major-mode-hydra"
    (major-mode-hydra-define go-mode nil
      ( "Test && Tag"
       (("g" go-gen-test-dwim "gen-dwim")
        ("m" go-test-current-file "test-file")
        ("." go-test-current-test "test-cur")
        ("t" go-tag-add "tag-add")
        ("T" go-tag-remove "tag-remove"))))))

;;; go-tag
(setq go-tag-args (list "-transform" "camelcase"))

(provide 'init-go)
