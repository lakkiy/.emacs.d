;;; init-dev.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;;; treesit

(setq treesit-language-source-alist
      '((toml . ("https://github.com/ikatyang/tree-sitter-toml"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))

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

;;; Code Formatter

;; apheleia
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


;;; Debugger

;; dape
(install-package 'dape)

;; Usually I use left window to show code
(setq dape-buffer-window-arrangement 'right)

;; Mode-line serves no purpose in REPL window.
(add-hook 'dape-repl-mode-hook #'hide-mode-line-mode)

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

;;; C
(setq c-default-style "linux"
      c-basic-offset 4)

;;; Compilation
;;
;; `compile'
(setq compilation-always-kill t
      compilation-ask-about-save nil
      compilation-scroll-output 'first-error)

;; Disable compiliation warnings
(setq warning-suppress-log-types '((comp)))

;;; Eglot
(setq eglot-autoshutdown t
      eglot-sync-connect nil ;; don't block of LSP connection attempts
      eglot-extend-to-xref t ;; make eglot manage file out of project by `xref-find-definitions'
      eglot-ignored-server-capabilites
      '(:documentHighlightProvider
        :documentFormattingProvider
        :documentRangeFormattingProvider
        :documentLinkProvider
        ;; 和 treesit 的缩进冲突
        :documentOnTypeFormattingProvider))
;; Eglot optimization
(setq jsonrpc-event-hook nil)
(setq eglot-events-buffer-size 0)
(setq eglot-report-progress nil)  ; Prevent Eglot minibuffer spam

;; Eglot optimization: Disable `eglot-events-buffer' to maintain consistent
;; performance in long-running Emacs sessions. By default, it retains 2,000,000
;; lines, and each new event triggers pretty-printing of the entire buffer,
;; leading to a gradual performance decline.
(setq eglot-events-buffer-config '(:size 0 :format full))


;; https://codeberg.org/mekeor/init/src/commit/11e3d86aa18090a5e3a6f0d29373c24373f29aaf/init.el#L813-L842
;; INFO: Translation:
;;   | JSON  | Eglot       |
;;   |-------+-------------|
;;   | true  | t           |
;;   | false | :json-false |
;;   | null  | nil         |
;;   | {}    | eglot-{}    |
(setq-default eglot-workspace-configuration
              '( :gopls ( :buildFlags ["-tags" "wireinject"]
                          :usePlaceholders t
                          :staticcheck t)
                 :pyright ( :checkOnlyOpenFiles t
                            :typeCheckingMode "basic")
                 :basedpyright ( :checkOnlyOpenFiles t
                                 :typeCheckingMode "basic")
                 ))

;; https://emacs-china.org/t/eglot-lsp-server-command/29566
(defun my/eglot-enable-command-provider (orig-fn server)
  "Unconditionally add :executeCommandProvider to Eglot client capabilities."
  (let ((original-capabilities (funcall orig-fn server)))
    ;; Add or update :executeCommandProvider at the top level
    (plist-put original-capabilities
               :executeCommandProvider '(:commands (:dynamicRegistration :json-false)))))
(advice-add 'eglot-client-capabilities :around #'my/eglot-enable-command-provider)

(defun my/eglot-execute-command (command)
  "Interactively execute a COMMAND supported by the current Eglot LSP server.
COMMAND is a string as advertised by the server. No arguments are passed."
  (interactive
   (let* ((server (eglot-current-server))
          (caps (eglot--capabilities server))
          (provider (plist-get caps :executeCommandProvider))
          (commands (and provider (plist-get provider :commands))))
     (list (completing-read "LSP Command: "
                            (or (cl-coerce commands 'list) '())
                            nil nil))))
  (eglot-execute (eglot-current-server) (list :command command)))

(with-eval-after-load 'eglot
  (keymap-set eglot-mode-map "M-RET" #'eglot-code-actions)
  (keymap-set eglot-mode-map "C-c r" #'eglot-rename)
  (keymap-set eglot-mode-map "M-'"   #'eglot-find-implementation)

  (add-to-list 'eglot-server-programs '(web-mode . ("svelteserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(sql-mode . ("sqls" "-config" "~/.config/sqls/config.yaml")))
  (add-to-list 'eglot-server-programs '(typst-ts-mode . ("typst-lsp")))
  (add-to-list 'eglot-server-programs '(org-mode . ("ltex-ls")))
  (add-to-list 'eglot-server-programs '(markdown-mode . ("ltex-ls")))
  (add-to-list 'eglot-server-programs '(message-mode . ("ltex-ls"))))

;;; Flymake
(add-hook 'prog-mode-hook #'flymake-mode)
(add-hook 'emacs-lisp-mode-hook #'(lambda ()
                                    (flymake-mode -1)))

(setq-default flymake-diagnostic-functions nil)

(defvar sekiro-flymake-mode-line-format `(:eval (sekiro-flymake-mode-line-format)))
(put 'sekiro-flymake-mode-line-format 'risky-local-variable t)
(defun sekiro-flymake-mode-line-format ()
  (let* ((counter (string-to-number
                   (nth 1
                        (cadr
                         (flymake--mode-line-counter :error)))))
         (sekiro-flymake (when (> counter 0)
                           'compilation-error)))
    (propertize
     "危"
     'face
     sekiro-flymake)))

(with-eval-after-load 'flymake
  (keymap-set flymake-mode-map "M-p" #'flymake-goto-prev-error)
  (keymap-set flymake-mode-map "M-n" #'flymake-goto-next-error)
  (add-to-list 'mode-line-misc-info
               `(flymake-mode (" [" sekiro-flymake-mode-line-format "] "))))

(add-hook 'flymake-mode-hook
          (lambda ()
            (add-hook 'eldoc-documentation-functions 'flymake-eldoc-function nil t)))

;;; Hide show
(defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))

(defface hideshow-border-face
  '((((background light))
     :background "rosy brown" :extend t)
    (t
     :background "sandy brown" :extend t))
  "Face used for hideshow fringe."
  :group 'hideshow)

(define-fringe-bitmap 'hideshow-folded-fringe
  (vector #b00000000
          #b00000000
          #b00000000
          #b11000011
          #b11100111
          #b01111110
          #b00111100
          #b00011000))

(defun hideshow-folded-overlay-fn (ov)
  "Display a folded region indicator with the number of folded lines."
  (when (eq 'code (overlay-get ov 'hs))
    (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
           (info (format " (%d)..." nlines)))
      ;; fringe indicator
      (overlay-put ov 'before-string (propertize " "
                                                 'display '(left-fringe hideshow-folded-fringe
                                                                        hideshow-border-face)))
      ;; folding indicator
      (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))

(setq hs-set-up-overlay #'hideshow-folded-overlay-fn)

(add-hook 'prog-mode-hook #'hs-minor-mode)

;;; GUD
(setq gud-highlight-current-line t)
(add-hook 'gud-mode-hook #'gud-tooltip-mode)

;;; Project
(setq project-vc-ignores '("target/" "bin/" "obj/")
      project-vc-extra-root-markers '(".project"
                                      "go.mod"
                                      "Cargo.toml"
                                      "project.clj"
                                      "pyproject.toml"
                                      "pyrightconfig.json"
                                      "package.json"))

(with-eval-after-load 'project
  ;; Use fd in `project-find-file'
  (when (executable-find "fd")
    (defun my/project-files-in-directory (dir)
      "Use `fd' to list files in DIR."
      (let* ((default-directory dir)
             (localdir (file-local-name (expand-file-name dir)))
             (command (format "fd -c never -H -t f -0 . %s" localdir)))
        (project--remote-file-names
         (sort (split-string (shell-command-to-string command) "\0" t)
               #'string<))))
    (cl-defmethod project-files ((project (head local)) &optional dirs)
      "Override `project-files' to use `fd' in local projects."
      (mapcan #'my/project-files-in-directory
              (or dirs (list (project-root project)))))))

;; Memoize current project
(defvar project-current-cache nil)
(defun memoize-project-current (orig &optional prompt directory)
  (memoize-remote (or directory
                      project-current-directory-override
                      default-directory)
                  'project-current-cache orig prompt directory))
(with-eval-after-load 'project
  (advice-add 'project-current :around #'memoize-project-current))

;;; Xref
;;
;; Enable completion in the minibuffer instead of the definitions buffer
(setq xref-show-definitions-function #'xref-show-definitions-completing-read
      xref-show-xrefs-function #'xref-show-definitions-completing-read)

;; Fix massed xref cross multiple project.
(setq xref-history-storage 'xref-window-local-history)

(add-hook 'xref-after-return-hook #'recenter)
(add-hook 'xref-after-jump-hook #'recenter)
(keymap-global-unset "C-<down-mouse-1>")
(keymap-global-set "C-<mouse-1>" #'xref-find-definitions-at-mouse)

(with-eval-after-load 'xref
  (setq xref-search-program (cond ((executable-find "rg") 'ripgrep)
                                  (t 'grep))))

;;; init-dev.el ends here
