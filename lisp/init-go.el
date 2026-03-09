;;; init-go.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;; git diff --name-only --cached | grep '\.go$' | xargs -I {} goimports -w {}

(install-package 'go-mode)
(install-package 'go-dlv)
(install-package 'go-gen-test)
(install-package 'gotest)
(install-package 'gotest-ts)
(install-package 'go-tag)
(install-package 'go-fill-struct)
(install-package 'flymake-go-staticcheck)

(setenv "GOPATH" (concat (getenv "HOME") "/.go"))

(setq gofmt-show-errors nil
      gofmt-command "goimports"
      go-test-verbose t
      ;; Do not cache test result.
      go-test-args "-count=1"
      go-tag-args (list "-transform" "camelcase"))

(when (executable-find "staticcheck")
  (add-hook 'go-mode-hook #'flymake-go-staticcheck-enable))

(with-eval-after-load 'go-mode
  (keymap-set go-mode-map "C-c t g" #'go-gen-test-dwim)
  (keymap-set go-mode-map "C-c t f" #'go-test-current-file)
  (keymap-set go-mode-map "C-c t t" #'go-test-current-test)
  (keymap-set go-mode-map "C-c t j" #'go-test-current-project)
  (keymap-set go-mode-map "C-c t b" #'go-test-current-benchmark)
  (keymap-set go-mode-map "C-c t c" #'go-test-current-coverage)
  (keymap-set go-mode-map "C-c t x" #'go-run)
  (keymap-set go-mode-map "C-c t a" #'go-tag-add)
  (keymap-set go-mode-map "C-c t r" #'go-tag-remove))

;; treesit
(dolist (lang '((go . ("https://github.com/tree-sitter/tree-sitter-go"))
                (gomod . ("https://github.com/camdencheek/tree-sitter-gomod.git"))
                (gowork . ("https://github.com/omertuc/tree-sitter-go-work.git"))))
  (add-to-list 'treesit-language-source-alist lang))

(setq go-ts-mode-indent-offset 4)

(when (treesit-available-p)
  (push '(go-mode . go-ts-mode) major-mode-remap-alist)
  (with-eval-after-load 'go-ts-mode
    (require 'go-mode)
    (setq go-ts-mode-hook go-mode-hook)
    (set-keymap-parent go-ts-mode-map go-mode-map)
    ;; Complete setup with keybindings and imenu for gotest-ts
    (add-hook 'go-ts-mode-hook #'gotest-ts-setup)))

;; https://github.com/chmouel/gotest-ts.el?tab=readme-ov-file#debugging-with-dape
;; debug test with gotest-ts
(defun my-dape-go-test-at-point ()
  (interactive)
  (dape (dape--config-eval-1
         `(modes (go-ts-mode)
                 ensure dape-ensure-command
                 fn dape-config-autoport
                 command "dlv"
                 command-args ("dap" "--listen" "127.0.0.1::autoport")
                 command-cwd dape-cwd-fn
                 port :autoport
                 :type "debug"
                 :request "launch"
                 :mode "test"
                 :cwd dape-cwd-fn
                 :program (lambda () (concat "./" (file-relative-name default-directory (funcall dape-cwd-fn))))
                 :args (lambda ()
                         (when-let* ((test-name (gotest-ts--build-test-pattern)))
                           (if test-name `["-test.run" ,test-name]
                             (error "No test selected"))))))))


;;; init-go.el ends here
