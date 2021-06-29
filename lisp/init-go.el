;;; -*- lexical-binding: t -*-

;; Install or update tools
(defvar go--tools '("golang.org/x/tools/cmd/goimports"
                    "github.com/go-delve/delve/cmd/dlv"
                    "github.com/josharian/impl"
                    "github.com/cweill/gotests/..."
                    "github.com/fatih/gomodifytags"
                    "golang.org/x/tools/cmd/guru"
                    "golang.org/x/tools/cmd/gorename"
                    "github.com/davidrjenni/reftools/cmd/fillstruct")
  "All necessary go tools.")

;; Do not use the -u flag for gopls, as it will update the dependencies to incompatible versions
;; https://github.com/golang/tools/blob/master/gopls/doc/user.md#installation
(defvar go--tools-no-update '("golang.org/x/tools/gopls@latest"))

(defun go-update-tools ()
    "Install or update go tools."
    (interactive)
    (unless (executable-find "go")
      (user-error "Unable to find `go' in `exec-path'!"))

    (message "Installing go tools...")
    (let ((proc-name "go-tools")
          (proc-buffer "*Go Tools*"))
      (dolist (pkg go--tools-no-update)
        (set-process-sentinel
         (start-process proc-name proc-buffer "go" "get" "-v" pkg)
         (lambda (proc _)
           (let ((status (process-exit-status proc)))
             (if (= 0 status)
                 (message "Installed %s" pkg)
               (message "Failed to install %s: %d" pkg status))))))

      (dolist (pkg go--tools)
        (set-process-sentinel
         (start-process proc-name proc-buffer "go" "get" "-u" "-v" pkg)
         (lambda (proc _)
           (let ((status (process-exit-status proc)))
             (if (= 0 status)
                 (message "Installed %s" pkg)
               (message "Failed to install %s: %d" pkg status))))))))

(eat-package go-playground
  :straight t
  :commands go-playground-mode)

(eat-package go-mode
  :straight t
  :init
  (setq gofmt-command "goimports")
  :hook
  (go-mode-hook . eglot-ensure)
  (before-save-hook . gofmt-before-save)
  (go-test-mode-hook . visual-line-mode)
  :config
  (with-eval-after-load "exec-path-from-shell"
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))
  ;; Try to install go tools if `gopls' is not found
  (unless (executable-find "gopls")
    (go-update-tools))
  (define-key go-mode-map (kbd "C-c t x") #'go-run))

(eat-package flymake-golangci
  ;; :straight t
  :after go-mode
  :init
  ;; FIXME
  (straight-use-package '(flymake-golangci :type git :host gitlab :repo "shackra/flymake-golangci"))
  :hook (go-mode-hook . flymake-golangci-load))

(eat-package gotest
  :straight t
  :after go-mode
  :config
  (define-key go-mode-map (kbd "C-c t m") #'go-test-current-file)
  (define-key go-mode-map (kbd "C-c t .") #'go-test-current-test))

(eat-package go-gen-test
  :straight t
  :after go-mode
  :config
  (define-key go-mode-map (kbd "C-c t g") #'go-gen-test-dwim))

(eat-package go-tag
  :straight t
  :after go-mode
  :config
  (define-key go-mode-map (kbd "C-c t t") #'go-tag-add)
  (define-key go-mode-map (kbd "C-c t T") #'go-tag-remove))

(eat-package go-fill-struct :straight t)
(eat-package go-guru :straight t)
(eat-package go-rename :straight t)
(eat-package go-dlv :straight t)
(eat-package go-impl :straight t)

(eat-package go-tag
  :straight t
  :init
  (setq go-tag-args (list "-transform" "camelcase")))

(eat-package go-test
  :straight t
  :init
  (setq go-test-verbose t))

(provide 'init-go)
