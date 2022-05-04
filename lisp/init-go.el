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
                    "github.com/davidrjenni/reftools/cmd/fillstruct"
                    "github.com/rogpeppe/godef" ;; for `godoc-at-point'
                    "github.com/golangci/golangci-lint/cmd/golangci-lint")
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

(eat-package go-mode
  :straight t
  :init
  (setq gofmt-command "goimports"
        gofmt-show-errors nil)
  :hook
  ;; HACK use `apheleia' instead
  ;; (before-save-hook . gofmt-before-save)
  (go-test-mode-hook . visual-line-mode))

;; FIXME not work for now
;; write own version
(eat-package flymake-golangci :straight t)

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
  (define-key go-mode-map (kbd "C-c t c") #'go-test-current-coverage)
  (define-key go-mode-map (kbd "C-c t x") #'go-run))

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

(eat-package go-fill-struct :straight t)
(eat-package go-guru :straight t)
(eat-package go-rename :straight t)
(eat-package go-impl :straight t)

(eat-package go-dlv :straight t)

(eat-package go-playground
  :straight t
  :commands go-playground-mode)

(provide 'init-go)
