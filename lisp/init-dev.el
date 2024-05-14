;;; -*- lexical-binding: t -*-


;;; direnv, load environment variables
(install-package 'envrc)
(add-hook 'after-init-hook #'envrc-global-mode)


;;; flymake, syntax check
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


;;; xref, cross reference
(add-hook 'xref-after-return-hook #'recenter)
(add-hook 'xref-after-jump-hook #'recenter)

(keymap-global-unset "C-<down-mouse-1>")
(keymap-global-set "C-<mouse-1>" #'xref-find-definitions-at-mouse)

(setq xref-show-xrefs-function #'xref-show-definitions-completing-read
      xref-show-definitions-function #'xref-show-definitions-completing-read
      ;; Fix massed xref cross multiple project.
      xref-history-storage 'xref-window-local-history)

(with-eval-after-load 'xref
  ;; Emacs 28+
  ;;
  ;; `project-find-regexp' can be faster when setting `xref-search-program' to
  ;;  `ripgrep'.
  (setq xref-search-program (cond ((executable-find "rg") 'ripgrep)
                                  (t 'grep))))

;;; eglot, lsp client
(install-package 'eglot-hierarchy "https://github.com/dolmens/eglot-hierarchy")
(setq eglot-events-buffer-size 0
      eglot-autoshutdown t
      eglot-sync-connect nil ;; don't block of LSP connection attempts
      eglot-extend-to-xref t ;; make eglot manage file out of project by `xref-find-definitions'
      eglot-ignored-server-capabilites
      '(:documentHighlightProvider
        :documentFormattingProvider
        :documentRangeFormattingProvider
        :documentLinkProvider
        ;; 和 treesit 的缩进冲突
        :documentOnTypeFormattingProvider))

(setq-default eglot-workspace-configuration
              '((:gopls
                 (:ui.completion.usePlaceholders . t)
                 (:ui.diagnostic.staticcheck . t)
                 ;; for I have to edit wire.go even ignore it in build time
                 (:build.buildFlags . ["-tags" "wireinject"]))))

(with-eval-after-load 'eglot
  (keymap-set eglot-mode-map "M-RET" #'eglot-code-actions)
  (keymap-set eglot-mode-map "C-c r" #'eglot-rename)
  (keymap-set eglot-mode-map "M-'"   #'eglot-find-implementation)

  (add-to-list 'eglot-server-programs '(sql-mode . ("sqls" "-config" "~/.config/sqls/config.yaml")))
  (add-to-list 'eglot-server-programs '(typst-ts-mode . ("typst-lsp")))
  (add-to-list 'eglot-server-programs '(org-mode . ("ltex-ls")))
  (add-to-list 'eglot-server-programs '(markdown-mode . ("ltex-ls")))
  (add-to-list 'eglot-server-programs '(message-mode . ("ltex-ls"))))


;;; gud; The unified debugger
(add-hook 'gud-mode-hook #'gud-tooltip-mode)
(setq gud-highlight-current-line t)


;;; dape, debug client use DAP
(install-package 'dape)

;; To use window configuration like gud (gdb-mi)
(setq dape-buffer-window-arrangement 'gud)

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
  (defun dape-go-test-rdir ()
    "Return the file directory relative to dape's cwd. This is used by Delve debugger."
    (if (string-suffix-p "_test.go" (buffer-name))
        (concat "./" (file-relative-name
                      default-directory (funcall dape-cwd-fn)))
      (error "Not test file")))

  (defun dape-go-test-args ()
    (let* ((test-name (save-excursion
                        (end-of-line)
                        (beginning-of-defun)
                        (when (re-search-forward "^func[[:space:]]+\\([[:alnum:]_]+\\)" nil t)
                          (match-string 1))))
           (test-regexp (concat "^" test-name "$")))
      (if test-name
          `["-test.run" ,test-regexp]
        (error "No test selected"))))

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
                 :args dape-go-test-args)))



;;; citre, ctags/gtag jump and complete
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


;;; apheleia, code formatter
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


;;; compile, custome compile buffer when `project-compile'
(setq compilation-always-kill t ; kill compilation process before starting another
      compilation-ask-about-save nil    ; save all buffers on `compile'
      compilation-scroll-output 'first-error)


;;; comment
(setq comment-empty-lines t)


;;; treesit, syntax highlight
(setq treesit-language-source-alist
      '((gomod . ("https://github.com/camdencheek/tree-sitter-gomod.git"))
        (toml . ("https://github.com/ikatyang/tree-sitter-toml"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml")))
      go-ts-mode-indent-offset 4)

(with-eval-after-load 'go-ts-mode
  (require 'go-mode)
  (setq go-ts-mode-hook go-mode-hook)
  (set-keymap-parent go-ts-mode-map go-mode-map))

(when (treesit-available-p)
  (push '(python-mode . python-ts-mode) major-mode-remap-alist)
  (push '(go-mode . go-ts-mode) major-mode-remap-alist))


;;; hideshow, code folding
(add-hook 'prog-mode-hook #'hs-minor-mode)

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


;;; pnui, struct editing
(install-package 'puni)
;; (:bind
;;  "M-r" 'puni-splice
;;  "C-(" 'puni-slurp-backward
;;  "C-)" 'puni-slurp-forward
;;  "C-{" 'puni-barf-backward
;;  "C-}" 'puni-barf-forward)


;;; eldoc-box, show eldoc in box
(setq eldoc-idle-delay 1
      eldoc-documentation-function 'eldoc-documentation-compose)
(install-package 'eldoc-box)
(setq eldoc-box-only-multi-line t)
(add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode)


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


;;; indent-tabrs, show indent level for yaml, python
(install-package 'indent-bars "https://github.com/jdtsmith/indent-bars.git")

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


;;; devdocs
(install-package 'devdocs)
(keymap-global-set "C-h D" #'devdocs-lookup)


;;; require langs
(install-package 'protobuf-mode)

(with-eval-after-load "protobuf-mode"
  (add-hook 'protobuf-mode-hook
            (lambda ()
              (setq imenu-generic-expression
                    '((nil "^[[:space:]]*\\(message\\|service\\|enum\\)[[:space:]]+\\([[:alnum:]]+\\)" 2))))))
(install-package 'yaml-mode)
(install-package 'toml-mode)

(require 'init-lisp)
(require 'init-go)
(require 'init-python)
(require 'init-c)
(require 'init-nix)
(require 'init-web)


(provide 'init-dev)
;;; init-dev.el ends here
