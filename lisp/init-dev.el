;;; -*- lexical-binding: t -*-

(eat-package flymake
  :init
  (autoload #'flymake "flymake" nil t)
  :config
  (define-key flymake-mode-map (kbd "C-c C-b") 'flymake-show-buffer-diagnostics)
  (define-key flymake-mode-map (kbd "C-c C-S-b") 'flymake-show-project-diagnostics))

(eat-package tree-sitter
  :straight t
  :init
  (eat-package tree-sitter-langs :straight t)
  :hook
  ((go-mode-hook) . tree-sitter-mode)
  (tree-sitter-after-on-hook . tree-sitter-hl-mode)
  :config
  (require 'tree-sitter-langs))

(eat-package  grammatical-edit
  :straight (grammatical-edit
             :type git
             :host github
             :repo "manateelazycat/grammatical-edit")
  :hook
  ((c-mode-common-hook
    c-mode-hook
    c++-mode-hook
    java-mode-hook
    haskell-mode-hook
    ;; emacs-lisp-mode-hook ;; use `puni'
    lisp-interaction-mode-hook
    lisp-mode-hook
    maxima-mode-hook
    ielm-mode-hook
    sh-mode-hook
    makefile-gmake-mode-hook
    php-mode-hook
    python-mode-hook
    js-mode-hook
    go-mode-hook
    qml-mode-hook
    jade-mode-hook
    css-mode-hook
    ruby-mode-hook
    coffee-mode-hook
    rust-mode-hook
    qmake-mode-hook
    lua-mode-hook
    swift-mode-hook)
   . (lambda () (grammatical-edit-mode 1)))
  :config
  (define-key grammatical-edit-mode-map (kbd "(") 'grammatical-edit-open-round)
  (define-key grammatical-edit-mode-map (kbd "[") 'grammatical-edit-open-bracket)
  (define-key grammatical-edit-mode-map (kbd "{") 'grammatical-edit-open-curly)
  (define-key grammatical-edit-mode-map (kbd ")") 'grammatical-edit-close-round)
  (define-key grammatical-edit-mode-map (kbd "]") 'grammatical-edit-close-bracket)
  (define-key grammatical-edit-mode-map (kbd "}") 'grammatical-edit-close-curly)
  (define-key grammatical-edit-mode-map (kbd "=") 'grammatical-edit-equal)
  (define-key grammatical-edit-mode-map (kbd "%") 'grammatical-edit-match-paren)
  (define-key grammatical-edit-mode-map (kbd "\"") 'grammatical-edit-double-quote)
  (define-key grammatical-edit-mode-map (kbd "SPC") 'grammatical-edit-space)
  (define-key grammatical-edit-mode-map (kbd "RET") 'grammatical-edit-newline)
  (define-key grammatical-edit-mode-map (kbd "M-o") 'grammatical-edit-backward-delete)
  (define-key grammatical-edit-mode-map (kbd "C-d") 'grammatical-edit-forward-delete)
  (define-key grammatical-edit-mode-map (kbd "C-k") 'grammatical-edit-kill)
  (define-key grammatical-edit-mode-map (kbd "M-\"") 'grammatical-edit-wrap-double-quote)
  (define-key grammatical-edit-mode-map (kbd "M-[") 'grammatical-edit-wrap-bracket)
  (define-key grammatical-edit-mode-map (kbd "M-{") 'grammatical-edit-wrap-curly)
  (define-key grammatical-edit-mode-map (kbd "M-(") 'grammatical-edit-wrap-round)
  (define-key grammatical-edit-mode-map (kbd "M-)") 'grammatical-edit-unwrap)
  (define-key grammatical-edit-mode-map (kbd "M-p") 'grammatical-edit-jump-right)
  (define-key grammatical-edit-mode-map (kbd "M-n") 'grammatical-edit-jump-left)
  (define-key grammatical-edit-mode-map (kbd "M-:") 'grammatical-edit-jump-out-pair-and-newline))

(eat-package clue
  :straight (clue :type git :host github :repo "AmaiKinono/clue"))

(eat-package citre
  :straight (citre :type git :host github :repo "universal-ctags/citre")
  :init
  (require 'citre-config)
  (global-set-key (kbd "C-x c j") 'citre-jump)
  (global-set-key (kbd "C-x c k") 'citre-jump-back)
  (global-set-key (kbd "C-x c p") 'citre-peek)
  (global-set-key (kbd "C-x c P") 'citre-ace-peek)
  (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)
  (setq citre-default-create-tags-file-location 'global-cache
        citre-use-project-root-when-creating-tags t
        citre-prompt-language-for-ctags-command t
        citre-auto-enable-citre-mode '(prog-mode))
  (eat-package citre-global
    :init
    (global-set-key (kbd "C-x c r") 'citre-jump-to-reference)
    (global-set-key (kbd "C-x c R") 'citre-ace-peek-references)
    ;; NOTE
    ;; Notice that GTAGSOBJDIRPREFIX must exist for gtags to use it. So you need to run:
    ;; $ mkdir -p ~/.cache/gtags/
    (global-set-key (kbd "C-x c U") 'citre-global-update-database)
    (with-eval-after-load 'citre-peek
      (define-key citre-peek-keymap (kbd "M-l r") 'citre-peek-through-references))
    :config
    (with-eval-after-load "exec-path-from-shell"
      (exec-path-from-shell-copy-envs '("GTAGSOBJDIRPREFIX" "GTAGSCONF" "GTAGSLABEL"))))
  :config
  (define-key citre-peek-keymap (kbd "M-l s") #'citre-peek-save-session)
  (define-key citre-peek-keymap (kbd "M-l h") #'citre-peek-chain-backward)
  (define-key citre-peek-keymap (kbd "M-l l") #'citre-peek-chain-forward)
  (define-key citre-peek-keymap (kbd "M-l j") #'citre-peek-prev-branch)
  (define-key citre-peek-keymap (kbd "M-l k") #'citre-peek-next-branch)
  (define-key citre-peek-keymap (kbd "M-l J") #'citre-peek-move-current-def-up)
  (define-key citre-peek-keymap (kbd "M-l K") #'citre-peek-move-current-def-down))

(eat-package flycheck
  :straight t
  ;; :hook (prog-mode-hook . flycheck-mode)
  :init
  (setq flycheck-temp-prefix ".flycheck"
        flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-emacs-lisp-load-path 'inherit
        flycheck-indication-mode (if (display-graphic-p)
                                     'left-fringe
                                   'left-margin))
  :config
  (define-key flycheck-mode-map (kbd "C-c C-b") 'flycheck-list-errors))

(eat-package flymake-flycheck
  :straight t
  :after flymake
  :config
  (setq-local
   flymake-diagnostic-functions (flymake-flycheck-all-chained-diagnostic-functions)))

(eat-package aggressive-indent
  :straight t
  :commands aggressive-indent-mode
  :hook ((emacs-lisp-mode-hook
          lisp-interaction-mode-hook
          scheme-mode-hook
          lisp-mode-hook)
         . aggressive-indent-mode))

;; HACK Doesn't support `TRAMP' now
(eat-package apheleia
  :straight t
  :hook (go-mode-hook . apheleia-mode)
  :config
  (setf (alist-get 'gofmt apheleia-formatters)
        '("goimports")))

(eat-package eldoc-overlay :straight t)
(eat-package devdocs :straight t)

(eat-package turbo-log
  :straight (turbo-log :host github :repo "artawower/turbo-log.el")
  :init
  (global-set-key (kbd "C-s-h") #'turbo-log-print-immediately)
  (global-set-key (kbd "C-s-g") #'turbo-log-delete-all-logs))

(eat-package eglot
  :straight t
  :commands
  eglot-ensure
  ;; enable `eglot' manually
  ;; :hook (go-mode-hook . eglot-ensure)
  :init
  (setq eglot-ignored-server-capabilites '(:documentHighlightProvider)
        ;; don't block of LSP connection attempts
        eglot-sync-connect nil
        ;; I will manage `company-capf' myself
        eglot-stay-out-of '(company)
        ;; make eglot manage file out of project by `xref-find-definitions'
        eglot-extend-to-xref t)
  :config
  (define-key eglot-mode-map (kbd "M-RET") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c h") 'eldoc)
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
			   '(rust-mode "rust-analyzer")))

(eat-package eglot-ltex
  :straight (eglot-ltex :type git :host github :repo "emacs-languagetool/eglot-ltex")
  :hook
  ((markdown-mode-hook org-mode-hook) . eglot-ltex)
  :init
  (defun eglot-ltex ()
    (interactive)
    (setq eglot-languagetool-server-path (expand-file-name "ltex-ls/" user-emacs-directory))
    (require 'eglot-ltex)
    (call-interactively #'eglot)))

(require 'init-go)
(require 'init-python)
(require 'init-web)

(provide 'init-dev)
