;;; -*- lexical-binding: t -*-

(eat-package aggressive-indent
  :straight t
  :commands aggressive-indent-mode
  :hook ((emacs-lisp-mode-hook
          lisp-interaction-mode-hook
          scheme-mode-hook
          lisp-mode-hook)
         . aggressive-indent-mode))

(eat-package apheleia
  :straight t
  :hook (go-mode-hook . apheleia-mode)
  :init
  (setq apheleia-remote-algorithm 'local)
  :config
  (setf (alist-get 'gofmt apheleia-formatters)
        '("goimports")))

(eat-package xscheme)

(eat-package dumb-jump
  :straight t
  ;; NOTE use `dumb-jump' as default xref backend
  ;; you can run `eglot' or `eat/citre-enable' to reset this
  :hook (xref-backend-functions . #'dumb-jump-xref-activate)
  :init
  (setq
   dumb-jump-force-searcher'rg
   dumb-jump-quiet t
   dumb-jump-aggressive t
   dumb-jump-selector 'completing-read))

(eat-package clue
  :straight (clue :type git :host github :repo "AmaiKinono/clue"))

(eat-package citre
  :straight (citre :type git :host github :repo "universal-ctags/citre")
  :init
  (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)
  (global-set-key (kbd "C-x c p") 'citre-peek)
  (setq citre-default-create-tags-file-location 'global-cache
        citre-use-project-root-when-creating-tags t
        citre-prompt-language-for-ctags-command t
        citre-auto-enable-citre-mode-modes '(prog-mode))
  (defun eat/citre-enable ()
    (interactive)
    (citre-mode 1)
    (require 'citre-config))
  (defun eat/citre-disable ()
    (interactive)
    (citre-mode 0)
    (remove-hook 'find-file-hook #'citre-auto-enable-citre-mode))
  (eat-package citre-global
    :init
    ;; NOTE
    ;; Notice that GTAGSOBJDIRPREFIX must exist for gtags to use it. So you need to run:
    ;; $ mkdir -p ~/.cache/gtags/
    (global-set-key (kbd "C-x c U") 'citre-global-update-database)
    (global-set-key (kbd "C-x c P") 'citre-peek-references)
    :config
    (setenv "GTAGSOBJDIRPREFIX" (concat (getenv "HOME") "/.cache/gtags"))
    (setenv "GTAGSCONF" (concat (getenv "HOME") "/.globalrc"))
    (setenv "GTAGSLABEL" "native-pygments")))

(eat-package flycheck
  :straight t
  :init
  (setq
   flycheck-check-syntax-automatically '(save mode-enabled)
   flycheck-indication-mode (if (display-graphic-p)
                                'right-fringe
                              'right-margin)
   flycheck-emacs-lisp-load-path 'inherit
   flycheck-temp-prefix ".flycheck"
   flycheck-disabled-checkers '(emacs-lisp
                                emacs-lisp-checkdoc
                                emacs-lisp-package
                                go-gofmt
                                go-golint
                                go-vet
                                go-build
                                go-test
                                go-errcheck
                                go-unconvert))
  :config
  (define-fringe-bitmap 'flycheck-fringe-bitmap-arrow
    [16 48 112 240 112 48 16] nil nil 'center)
  (flycheck-redefine-standard-error-levels "⏴" 'flycheck-fringe-bitmap-arrow)
  (define-key flycheck-mode-map [remap flymake-goto-prev-error] #'flycheck-previous-error)
  (define-key flycheck-mode-map [remap flymake-goto-next-error] #'flycheck-next-error))

(eat-package flymake-flycheck
  :straight t
  :init
  (defun eat/flymake-flycheck-enable ()
    (interactive)
    (setq-local flymake-diagnostic-functions
                (append flymake-diagnostic-functions
                        (flymake-flycheck-all-chained-diagnostic-functions))))
  (add-hook 'flymake-mode-hook 'eat/flymake-flycheck-enable))

(eat-package eglot
  :straight t
  :commands eglot-ensure
  :init
  (setq
   eglot-events-buffer-size 0
   eglot-sync-connect nil       ;; don't block of LSP connection attempts
   eglot-stay-out-of '(company) ;; I will manage these myself
   eglot-extend-to-xref t       ;; make eglot manage file out of project by `xref-find-definitions'
   eglot-ignored-server-capabilites '(:documentHighlightProvider))
  :config
  ;; keybindings
  (define-key eglot-mode-map (kbd "M-RET") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "M-'") 'eglot-find-implementation)
  ;; TODO code actions
  (add-to-list 'eglot-server-programs
               '(sql-mode . ("sqls" "-config" "~/.config/sqls/config.yaml")))
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
			   '(rust-mode "rust-analyzer")))

;; this need pip install epc
(eat-package lsp-bridge
  :straight (lsp-bridge
             :type git
             :host github
             :repo "manateelazycat/lsp-bridge"
             :files (:defaults "*.py" "core/*" "langserver/*")
             ;; do not generate autoload file
             ;; it has an annoying hook to `post-command-hook'
             ;; and emacs have to load this package or
             ;; it will cause an error on startup
             :build (:not autoloads))
  :commands lsp-bridge-mode
  :init
  (setq lsp-bridge-completion-provider 'corfu)
  :config
  (define-key lsp-bridge-mode-map (kbd "M-.") #'lsp-bridge-find-def)
  (define-key lsp-bridge-mode-map (kbd "M-,") #'lsp-bridge-return-from-def)
  (define-key lsp-bridge-mode-map (kbd "M-?") #'lsp-bridge-find-references)
  (define-key lsp-bridge-mode-map (kbd "M-'") #'lsp-bridge-find-impl)
  (define-key lsp-bridge-mode-map (kbd "C-x 4 .") #'lsp-bridge-find-def-other-window)
  (define-key lsp-bridge-mode-map (kbd "C-c r") #'lsp-bridge-rename))

(require 'init-go)
(require 'init-clojure)

(provide 'init-dev)
