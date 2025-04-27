;;; init-dev.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;;; eglot-booster
(install-package 'eglot-booster "https://github.com/jdtsmith/eglot-booster")
(when (executable-find "emacs-lsp-booster")
  (setq eglot-booster-no-remote-boost t)
  (add-hook 'after-init-hook #'eglot-booster-mode))

;;; dumb-jump
;;
;; As default xref backend function.
(install-package 'dumb-jump)

(setq dumb-jump-quiet t
      dumb-jump-prefer-searcher 'rg
      ;; If force searcher is not set, it will default to using git-grep
      ;; in a git project, and git-grep just don't work at all.
      dumb-jump-force-searcher 'rg)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;;; citre
;;
;; Use ctags/gtag to jump and complete.
(install-package 'citre)

(setq citre-default-create-tags-file-location 'global-cache
      citre-use-project-root-when-creating-tags t
      citre-prompt-language-for-ctags-command t
      citre-auto-enable-citre-mode-modes '(prog-mode))

(keymap-global-set "C-x c u" #'citre-update-this-tags-file)
(keymap-global-set "C-x c U" #'citre-global-update-database)

(with-eval-after-load 'citre
  ;; Notice that GTAGSOBJDIRPREFIX must exist for gtags to use.
  (when (not (file-exists-p (concat (getenv "HOME") "/.cache/gtags")))
    (make-directory (concat (getenv "HOME") "/.cache/gtags") t))
  (with-eval-after-load 'cc-mode (require 'citre-lang-c))
  (with-eval-after-load 'dired (require 'citre-lang-fileref))
  (with-eval-after-load 'verilog-mode (require 'citre-lang-verilog)))

(with-eval-after-load 'citre-global
  (setenv "GTAGSOBJDIRPREFIX" (concat (getenv "HOME") "/.cache/gtags"))
  (setenv "GTAGSCONF" (concat (getenv "HOME") "/.globalrc"))
  (setenv "GTAGSLABEL" "native-pygments"))

(with-eval-after-load 'citre-peek
  (keymap-set citre-peek-keymap "M-l r" 'citre-peek-through-references))

;;; apheleia
;;
;; formatter
(install-package 'apheleia)

;; Don't format remote file on save, use func to format project's all
;; changed file, for example
;; git diff --name-only --cached | grep '\.go$' | xargs -I {} goimports -w {}
(setq apheleia-remote-algorithm 'cancel)

(add-hook 'go-mode-hook #'apheleia-mode)
(add-hook 'python-base-mode-hook #'apheleia-mode)
(add-hook 'd2-mode-hook #'apheleia-mode)

(with-eval-after-load 'apheleia
  ;; d2
  (when (executable-find "d2")
    (push '(d2fmt "d2" "fmt" file) apheleia-formatters)
    (push '(d2-mode . d2fmt) apheleia-mode-alist))
  ;; python
  (when (executable-find "ruff")
    (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
    (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff)))
  ;; go
  (when (executable-find "goimports")
    (setf (alist-get 'go-mode apheleia-mode-alist) '(goimports))
    (setf (alist-get 'go-ts-mode apheleia-mode-alist) '(goimports))))

;;; dape
(install-package 'dape)

;; Usually I use left window to show code
(setq dape-buffer-window-arrangement 'right)

(defun my/dape-setup ()
  (require 'dape)
  ;; Global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode)
  ;; Load breakpoints on startup
  (dape-breakpoint-load))
(add-hook 'prog-mode-hook #'my/dape-setup)

(with-eval-after-load 'dape
  ;; Save breakpoints on quit
  (add-hook 'kill-emacs-hook #'dape-breakpoint-save)
  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-compile-hooks 'kill-buffer)
  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-on-start-hooks (lambda () (save-some-buffers t t)))

  ;; Support debug go unit test
  ;; Also see https://github.com/microsoft/debugpy/wiki/Debugging-over-SSH
  (defun dape-go-test-rdir ()
    "Return the file directory relative to dape's cwd. This is used by Delve debugger."
    (if (string-suffix-p "_test.go" (buffer-name))
        (concat "./" (file-relative-name
                      default-directory (funcall dape-cwd-fn)))
      (error "Not test file")))
  (defun dape-go-test-name ()
    (require 'which-func)
    (if-let* ((file-name (buffer-file-name))
              ((string-suffix-p "_test.go" file-name))
              (fn-name (which-function)))
        `["-test.run" ,(concat "^" (car (split-string (substring-no-properties fn-name))) "$")]
      []))
  ;; https://github.com/go-delve/delve/blob/master/Documentation/usage/dlv_dap.md
  ;; https://github.com/go-delve/delve/blob/master/Documentation/usage/dlv_test.md
  (add-to-list 'dape-configs
               `(dlv-test
                 modes (go-mode go-ts-mode)
                 ensure dape-ensure-command
                 command "dlv"
                 command-args ("dap" "--listen" "127.0.0.1::autoport")
                 command-cwd dape-command-cwd
                 port :autoport
                 :request "launch"
                 :mode "test"
                 :type "debug"
                 :cwd dape-cwd-fn
                 :program dape-go-test-rdir
                 :args dape-go-test-name)))

;;; direnv
(install-package 'envrc)
(add-hook 'after-init-hook #'envrc-global-mode)

;;; treesit
(setq treesit-language-source-alist
      '((gomod . ("https://github.com/camdencheek/tree-sitter-gomod.git"))
        (toml . ("https://github.com/ikatyang/tree-sitter-toml"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src")))
      go-ts-mode-indent-offset 4)

(with-eval-after-load 'go-ts-mode
  (require 'go-mode)
  (setq go-ts-mode-hook go-mode-hook)
  (set-keymap-parent go-ts-mode-map go-mode-map))

(when (treesit-available-p)
  (push '(python-mode . python-ts-mode) major-mode-remap-alist)
  (push '(go-mode . go-ts-mode) major-mode-remap-alist))

;;; hideshow
;;; pnui
(install-package 'puni)
;; (:bind
;;  "M-r" 'puni-splice
;;  "C-(" 'puni-slurp-backward
;;  "C-)" 'puni-slurp-forward
;;  "C-{" 'puni-barf-backward
;;  "C-}" 'puni-barf-forward)

;;; eldoc-box
(install-package 'eldoc-box)
(setq eldoc-box-only-multi-line t)
(add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode)

;;; indent-tabrs
(install-package 'indent-bars)

(add-hook 'python-base-mode-hook #'indent-bars-mode)
(add-hook 'yaml-mode-hook #'indent-bars-mode)
(add-hook 'yaml-ts-mode-hook #'indent-bars-mode)

;; https://github.com/jdtsmith/indent-bars/blob/main/examples.md
;; Minimal colorpop but disable color by depth
(setq indent-bars-color '(highlight :face-bg t :blend 0.15)
      indent-bars-pattern "."
      indent-bars-width-frac 0.1
      indent-bars-pad-frac 0.1
      indent-bars-zigzag nil
      indent-bars-color-by-depth nil
      indent-bars-highlight-current-depth '(:blend 0.5)
      indent-bars-display-on-blank-lines nil)

(with-eval-after-load 'indent-bars
  (when (treesit-available-p)
    (require 'indent-bars-ts)
    (setq indent-bars-treesit-support t
          indent-bars-treesit-ignore-blank-lines-types '("module")
          indent-bars-treesit-wrap '((python argument_list parameters
				                             list list_comprehension
				                             dictionary dictionary_comprehension
				                             parenthesized_expression subscript)))))

;;; misc
(install-package 'yaml-mode)
(install-package 'toml-mode)
(install-package 'nginx-mode)
(install-package 'just-mode)
(install-package 'justl)
(install-package 'rust-mode)
(install-package 'racket-mode)
(install-package 'nix-mode)
(install-package 'protobuf-mode)

(with-eval-after-load "protobuf-mode"
  (add-hook 'protobuf-mode-hook
            (lambda ()
              (setq imenu-generic-expression
                    '((nil "^[[:space:]]*\\(message\\|service\\|enum\\)[[:space:]]+\\([[:alnum:]]+\\)" 2))))))

;;; init-dev.el ends here
