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
                    "github.com/rogpeppe/godef")
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

(defun my/format-go ()
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (shell-command "git diff --name-only --cached | grep '\.go$' | xargs -I {} goimports -w {}")))
(keymap-set project-prefix-map "t" #'my/format-go)

(install-package 'go-mode)
(install-package 'go-gen-test)
(install-package 'go-dlv)
(install-package 'go-fill-struct)
(install-package 'go-impl)
(install-package 'gotest)
(install-package 'go-tag)

(install-package 'flymake-go-staticcheck)
(when (executable-find "staticcheck")
  (add-hook 'go-mode-hook #'flymake-go-staticcheck-enable))

(setq gofmt-command "goimports"
      gofmt-show-errors nil)
(add-hook 'go-test-mode-hook #'visual-line-mode)

(setq go-test-verbose t
      ;; Do not cache test result.
      go-test-args "-count=1")

(setq go-tag-args (list "-transform" "camelcase"))

(with-eval-after-load 'go-mode
  (keymap-set go-mode-map "C-c t g" #'go-gen-test-dwim)
  (keymap-set go-mode-map "C-c t t" #'go-test-current-test)
  (keymap-set go-mode-map "C-c t a" #'go-tag-add)
  (keymap-set go-mode-map "C-c t r" #'go-tag-remove))

;;; init-go.el ends here
(provide 'init-go)
