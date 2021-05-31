;;; -*- lexical-binding: t -*-

(straight-use-package 'go-mode)
(straight-use-package 'gotest)
(straight-use-package 'go-gen-test)
(straight-use-package 'go-dlv)
(straight-use-package 'go-fill-struct)
(straight-use-package 'go-impl)
(straight-use-package 'go-tag)
(straight-use-package 'go-playground)
(straight-use-package '(flymake-golangci :type git :host gitlab :repo "shackra/flymake-golangci"))

;; Install or update tools
(defvar go--tools '("golang.org/x/tools/cmd/goimports"
                    "golang.org/x/tools/gopls"
                    "github.com/go-delve/delve/cmd/dlv"
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
  (let ((proc-name "go-tools")
        (proc-buffer "*Go Tools*"))
    (dolist (pkg go--tools)
      (set-process-sentinel
       (start-process proc-name proc-buffer "go" "get" "-u" "-v" pkg)
       (lambda (proc _)
         (let ((status (process-exit-status proc)))
           (if (= 0 status)
               (message "Installed %s" pkg)
             (message "Failed to install %s: %d" pkg status))))))))

;;; go-playground
(autoload 'go-playground-mode "go-playground" nil t)

;;; go-mode
(setq gofmt-command "goimports")

(with-eval-after-load "go-mode"
  (with-eval-after-load "exec-path-from-shell"
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))
  ;; Try to install go tools if `gopls' is not found
  (unless (executable-find "gopls")
    (go-update-tools))
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
  (define-key go-mode-map (kbd "C-c t x") #'go-run))

;;; go-tag
(setq go-tag-args (list "-transform" "camelcase"))

;;; go-test
(setq go-test-verbose t)

(provide 'init-go)
