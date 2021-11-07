;;; -*- lexical-binding: t -*-

;; Install or update tools
(defvar go--tools '("golang.org/x/tools/gopls"
                    "golang.org/x/tools/cmd/goimports"
                    "honnef.co/go/tools/cmd/staticcheck"
                    "github.com/go-delve/delve/cmd/dlv"
                    "github.com/zmb3/gogetdoc"
                    "github.com/josharian/impl"
                    "github.com/cweill/gotests/..."
                    "github.com/fatih/gomodifytags"
                    "github.com/davidrjenni/reftools/cmd/fillstruct")
  "All necessary go tools.")

(defun go-update-tools ()
  "Install or update go tools."
  (interactive)
  (unless (executable-find "go")
    (user-error "Unable to find `go' in `exec-path'!"))

  (message "Installing go tools...")
  (dolist (pkg go--tools)
    (set-process-sentinel
     (start-process "go-tools" "*Go Tools*" "go" "install" "-v" "-x" (concat pkg "@latest"))
     (lambda (proc _)
       (let ((status (process-exit-status proc)))
         (if (= 0 status)
             (message "Installed %s" pkg)
           (message "Failed to install %s: %d" pkg status)))))))

(eat-package go-playground
  :straight t
  :commands go-playground-mode)

;; FIXME https://github.com/golang/go/issues/29202
;; Add this to `.dir-locals.el' to project root dir
;; ((go-mode . ((eglot-workspace-configuration . ((:gopls . (:buildFlags ["-tags=debug"])))))))
(eat-package go-mode
  :straight t
  :init
  (setq gofmt-command "goimports"
        gofmt-show-errors nil)
  :hook
  (before-save-hook . gofmt-before-save)
  (go-test-mode-hook . visual-line-mode)
  :config
  (with-eval-after-load "exec-path-from-shell"
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))
  ;; Look for the nearest parent `go.mod' file (that is, the root of the Go module) as the project root.
  (with-eval-after-load "project"
    (defun project-find-go-module (dir)
      (when-let ((root (locate-dominating-file dir "go.mod")))
        (cons 'go-module root)))
    (cl-defmethod project-root ((project (head go-module)))
      (cdr project))
    (add-hook 'project-find-functions #'project-find-go-module))
  (define-key go-mode-map (kbd "C-c t x") #'go-run))

(eat-package flymake-golangci
  :straight (flymake-golangci :type git :host gitlab :repo "shackra/flymake-golangci")
  :after go-mode
  :init
  :hook (go-mode-hook . flymake-golangci-load))

(eat-package flycheck-golangci-lint
  :straight t
  ;; :after go-mode flycheck
  ;; :hook (go-mode . (lambda ()
  ;;                    "Enable golangci-lint."
  ;;                    (setq flycheck-disabled-checkers '(go-gofmt
  ;;                                                       go-golint
  ;;                                                       go-vet
  ;;                                                       go-build
  ;;                                                       go-test
  ;;                                                       go-errcheck))
  ;;                    (flycheck-golangci-lint-setup)))
  )

(eat-package gotest
  :straight t
  :after go-mode
  :init
  (setq go-test-verbose t
        ;; do not cache test
        go-test-args "-count=1")
  :config
  (define-key go-mode-map (kbd "C-c t f") #'go-test-current-file)
  (define-key go-mode-map (kbd "C-c t t") #'go-test-current-test)
  (define-key go-mode-map (kbd "C-c t j") #'go-test-current-project)
  (define-key go-mode-map (kbd "C-c t b") #'go-test-current-benchmark)
  (define-key go-mode-map (kbd "C-c t c") #'go-test-current-coverage))

(eat-package go-gen-test
  :straight t
  :after go-mode
  :config
  (define-key go-mode-map (kbd "C-c t g") #'go-gen-test-dwim))

(eat-package go-tag
  :straight t
  :after go-mode
  :init
  (setq go-tag-args (list "-transform" "camelcase"))
  :config
  (define-key go-mode-map (kbd "C-c t a") #'go-tag-add)
  (define-key go-mode-map (kbd "C-c t r") #'go-tag-remove))

(eat-package go-fill-struct :straight t :after go-mode)
(eat-package go-guru :straight t :after go-mode)
(eat-package go-rename :straight t :after go-mode)
(eat-package go-impl :straight t :after go-mode)
;; FIXME bug in emacs-28
(eat-package go-dlv :straight t)

(provide 'init-go)
