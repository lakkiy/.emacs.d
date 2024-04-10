;;; -*- lexical-binding: t -*-

;;; compile
(setq compilation-always-kill t       ; kill compilation process before starting another
      compilation-ask-about-save nil  ; save all buffers on `compile'
      compilation-scroll-output 'first-error)

;;; comment
(setq comment-empty-lines t)

;;; xref
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

;;; eglot
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

  (add-to-list 'eglot-server-programs '(rust-mode "rust-analyzer"))
  (add-to-list 'eglot-server-programs '(sql-mode . ("sqls" "-config" "~/.config/sqls/config.yaml")))
  (add-to-list 'eglot-server-programs '(nix-mode . ("nixd")))
  (add-to-list 'eglot-server-programs '(typst-ts-mode . ("typst-lsp")))
  (add-to-list 'eglot-server-programs '(org-mode . ("ltex-ls")))
  (add-to-list 'eglot-server-programs '(markdown-mode . ("ltex-ls")))
  (add-to-list 'eglot-server-programs '(message-mode . ("ltex-ls"))))

;;; treesit
(setq treesit-language-source-alist
      '((gomod . ("https://github.com/camdencheek/tree-sitter-gomod.git"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml")))
      go-ts-mode-indent-offset 4)

(with-eval-after-load 'go-ts-mode
  (require 'go-mode)

  (setq go-ts-mode-hook go-mode-hook)
  (set-keymap-parent go-ts-mode-map go-mode-map))

(when (treesit-available-p)
  (push '(python-mode . python-ts-mode) major-mode-remap-alist)
  (push '(go-mode . go-ts-mode) major-mode-remap-alist)

  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode)))

;;; hideshow
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

;;; flymake
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

;;; eldoc-box
(setq eldoc-idle-delay 1
      eldoc-documentation-function 'eldoc-documentation-compose)
(install-package 'eldoc-box)
(setq eldoc-box-only-multi-line t)
(add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode)

;;; vundo
(install-package 'vundo)
(install-package 'undo-hl "https://github.com/casouri/undo-hl.git")
(add-hook 'prog-mode-hook #'undo-hl-mode)
(add-hook 'conf-mode-hook #'undo-hl-mode)
(keymap-global-set "C-z" #'vundo)

;;; direnv
(install-package 'envrc)
(add-hook 'after-init-hook #'envrc-global-mode)

;;; pnui
(install-package 'puni)
;; (:bind
;;  "M-r" 'puni-splice
;;  "C-(" 'puni-slurp-backward
;;  "C-)" 'puni-slurp-forward
;;  "C-{" 'puni-barf-backward
;;  "C-}" 'puni-barf-forward)

;;; eglot-hierarchy
(install-package 'eglot-hierarchy "https://github.com/dolmens/eglot-hierarchy")

;;; dape
(install-package 'dape)
(autoload #'dape-toggle-breakpoint "dape" nil t)

;;; apheleia
(install-package 'apheleia)

;; Don't format remote file on save, use func to format project's all
;; changed file, for example
;; git diff --name-only --cached | grep '\.go$' | xargs -I {} goimports -w {}
(setq apheleia-remote-algorithm 'cancel)

(add-hook 'go-mode-hook #'apheleia-mode)
(add-hook 'd2-mode-hook #'apheleia-mode)

(with-eval-after-load 'apheleia
  ;; Add support for d2.
  (push '(d2fmt "d2" "fmt" file) apheleia-formatters)
  (push '(d2-mode . d2fmt) apheleia-mode-alist)

  (push '(go-ts-mode . gofmt) apheleia-mode-alist)
  (when (executable-find "goimports")
    (setf (alist-get 'gofmt apheleia-formatters)
          '("goimports"))))

;;; devdocs
(install-package 'devdocs)
(keymap-global-set "C-h D" #'devdocs-lookup)

;; Jump to definition
;;
;; As default xref backend function.
(install-package 'dumb-jump)

(setq-default xref-backend-functions '(dumb-jump-xref-activate))

(keymap-global-set "M-g j" #'dumb-jump-go)
(keymap-global-set "M-g J" #'dumb-jump-go-other-window)

(setq dumb-jump-quiet t
      dumb-jump-aggressive t
      dumb-jump-selector 'completing-read)

;;; citre
;;
;; Use ctags/gtag to jump and complete.
(install-package 'citre)

(keymap-global-set "C-x c j" #'citre-jump)
(keymap-global-set "C-x c u" #'citre-update-this-tags-file)
(keymap-global-set "C-x c p" #'citre-peek)
(keymap-global-set "C-x c U" #'citre-global-update-database)
(keymap-global-set "C-x c r" #'citre-jump-to-reference)

(with-eval-after-load 'citre
  (keymap-global-set "C-x c J" #'citre-jump-back)
  ;; NOTE
  ;; Notice that GTAGSOBJDIRPREFIX must exist for gtags to use it. So you need to run:
  ;; $ mkdir -p ~/.cache/gtags/
  (keymap-global-set "C-x c P" #'citre-ace-peek-references)
  (setq citre-default-create-tags-file-location 'global-cache
        citre-use-project-root-when-creating-tags t
        citre-prompt-language-for-ctags-command t
        citre-auto-enable-citre-mode-modes '(prog-mode))
  (with-eval-after-load 'cc-mode (require 'citre-lang-c))
  (with-eval-after-load 'dired (require 'citre-lang-fileref))
  (with-eval-after-load 'verilog-mode (require 'citre-lang-verilog)))

(with-eval-after-load 'citre-global
  (setenv "GTAGSOBJDIRPREFIX" (concat (getenv "HOME") "/.cache/gtags"))
  (setenv "GTAGSCONF" (concat (getenv "HOME") "/.globalrc"))
  (setenv "GTAGSLABEL" "native-pygments"))

(with-eval-after-load 'citre-peek
  (keymap-set citre-peek-keymap "M-l r" 'citre-peek-through-references))

;;; protobuf
(install-package 'protobuf-mode)

(with-eval-after-load "protobuf-mode"
  (add-hook 'protobuf-mode-hook
            (lambda ()
              (setq imenu-generic-expression
                    '((nil "^[[:space:]]*\\(message\\|service\\|enum\\)[[:space:]]+\\([[:alnum:]]+\\)" 2))))))

;;; indent-tabrs
(install-package 'indent-bars "https://github.com/jdtsmith/indent-bars.git")

(add-hook 'python-base-mode-hook #'indent-bars-mode)
(add-hook 'yaml-mode-hook #'indent-bars-mode)
(add-hook 'yaml-ts-mode-hook #'indent-bars-mode)

(setq indent-bars-color '(highlight :face-bg t :blend 0.15)
      indent-bars-pattern "."
      indent-bars-width-frac 0.1
      indent-bars-pad-frac 0.1
      indent-bars-zigzag nil
      indent-bars-color-by-depth nil
      indent-bars-display-on-blank-lines nil)
(setq indent-bars-treesit-support (treesit-available-p)
      ;; indent-bars-prefer-character "│"
      indent-bars-highlight-current-depth '(:face default :blend 0.3)
      ;; https://github.com/jdtsmith/indent-bars#configuring-tree-sitter
      indent-bars-treesit-ignore-blank-lines-types '("module")
      indent-bars-treesit-wrap '((python argument_list parameters
				                         list list_comprehension
				                         dictionary dictionary_comprehension
				                         parenthesized_expression subscript)))

;;; lsp-bridge, mainly for editing remote files.
(install-package 'lsp-bridge "https://github.com/manateelazycat/lsp-bridge")
(install-package 'acm-terminal "https://github.com/twlz0ne/acm-terminal")
(install-package 'flymake-bridge "https://github.com/liuyinz/flymake-bridge")

(setq lsp-bridge-c-lsp-server "ccls"
      acm-enable-tabnine nil
      ;; use `lsp-bridge-enable-with-tramp' or tramp
      ;; clone lsp-bridge repo to home
      lsp-bridge-enable-with-tramp t
      lsp-bridge-remote-start-automatically t
      ;; NOTE otherwise this may cause lsp-bridge-ref buffer didn't show
      window-resize-pixelwise nil)

(defun my/lsp-bridge-mode-setup ()
  (interactive)
  (flymake-bridge-setup)
  ;; Disable corfu since lsp-bridge use acm.
  (ignore-errors
    (company-mode -1)
    (corfu-mode -1))
  ;; Use tab to jump to next field but do complete when there's acm complete.
  (with-eval-after-load 'yasnippet
    (define-key yas-keymap (kbd "<tab>") 'acm-complete-or-expand-yas-snippet)
    (define-key yas-keymap (kbd "TAB") 'acm-complete-or-expand-yas-snippet))
  ;; make completion work in terminal
  (unless (display-graphic-p)
    (with-eval-after-load 'acm
      (require 'acm-terminal))))

(with-eval-after-load 'lsp-bridge
  (add-hook 'lsp-bridge-mode-hook #'my/lsp-bridge-mode-setup)

  (keymap-set lsp-bridge-mode-map "M-."     #'lsp-bridge-find-def)
  (keymap-set lsp-bridge-mode-map "C-x 4 ." #'lsp-bridge-find-def-other-window)
  (keymap-set lsp-bridge-mode-map "M-,"     #'lsp-bridge-find-def-return)
  (keymap-set lsp-bridge-mode-map "M-?"     #'lsp-bridge-find-references)
  (keymap-set lsp-bridge-mode-map "M-'"     #'lsp-bridge-find-impl)
  (keymap-set lsp-bridge-mode-map "C-c r"   #'lsp-bridge-rename)
  (keymap-set lsp-bridge-mode-map "M-RET"   #'lsp-bridge-code-action)
  (keymap-set lsp-bridge-ref-mode-map "p"   #'lsp-bridge-ref-jump-prev-file)
  (keymap-set lsp-bridge-ref-mode-map "h"   #'lsp-bridge-ref-jump-prev-keyword)
  (keymap-set lsp-bridge-ref-mode-map "t"   #'lsp-bridge-ref-jump-next-keyword)
  (keymap-set lsp-bridge-ref-mode-map "n"   #'lsp-bridge-ref-jump-next-file)
  (keymap-set lsp-bridge-ref-mode-map "j" nil)
  (keymap-set lsp-bridge-ref-mode-map "k" nil)
  (keymap-set lsp-bridge-ref-mode-map "h" nil)
  (keymap-set lsp-bridge-ref-mode-map "l" nil))

;;; require langs
(require 'init-lisp)
(require 'init-go)
(require 'init-python)
(require 'init-c)
(require 'init-nix)
(require 'init-web)

(provide 'init-dev)
