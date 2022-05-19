;;; -*- lexical-binding: t -*-

(eat-package tree-sitter
  :straight t
  :init
  (eat-package tree-sitter-langs :straight t)
  :hook
  ((go-mode-hook) . tree-sitter-mode)
  (tree-sitter-after-on-hook . tree-sitter-hl-mode)
  :config
  (require 'tree-sitter-langs))

(eat-package grammatical-edit
  :straight (grammatical-edit
             :type git
             :host github
             :repo "manateelazycat/grammatical-edit")
  :hook
  ;; NOTE not work well in sh mode, and tramp, see tramp debug buffer
  ((python-mode-hook
    js-mode-hook
    go-mode-hook)
   . (lambda () (grammatical-edit-mode 1)))
  (go-dot-mod-mode-hook . (lambda () (grammatical-edit-mode -1)))
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
  (define-key grammatical-edit-mode-map (kbd "'") 'grammatical-edit-single-quote)

  (define-key grammatical-edit-mode-map (kbd "SPC") 'grammatical-edit-space)
  (define-key grammatical-edit-mode-map (kbd "RET") 'grammatical-edit-newline)

  (define-key grammatical-edit-mode-map (kbd "M-o") 'grammatical-edit-backward-delete)
  (define-key grammatical-edit-mode-map (kbd "C-d") 'grammatical-edit-forward-delete)
  (define-key grammatical-edit-mode-map (kbd "C-k") 'grammatical-edit-kill)

  (define-key grammatical-edit-mode-map (kbd "M-p") 'grammatical-edit-jump-right)
  (define-key grammatical-edit-mode-map (kbd "M-n") 'grammatical-edit-jump-left)
  (define-key grammatical-edit-mode-map (kbd "M-:") 'grammatical-edit-jump-out-pair-and-newline)

  (define-key grammatical-edit-mode-map (kbd "C-j") 'grammatical-edit-jump-up))

(eat-package clue
  :straight (clue :type git :host github :repo "AmaiKinono/clue"))

(eat-package citre
  :straight (citre :type git :host github :repo "universal-ctags/citre")
  :init
  (global-set-key (kbd "C-x c j") 'citre-jump)
  (global-set-key (kbd "C-x c k") 'citre-jump-back)
  (global-set-key (kbd "C-x c p") 'citre-peek)
  (global-set-key (kbd "C-x c P") 'citre-ace-peek)
  (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)
  (setq citre-default-create-tags-file-location 'global-cache
        citre-use-project-root-when-creating-tags t
        citre-prompt-language-for-ctags-command t
        citre-auto-enable-citre-mode-modes '(prog-mode))
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
    (setenv "GTAGSOBJDIRPREFIX" (concat (getenv "HOME") "/.cache/gtags"))
    (setenv "GTAGSCONF" (concat (getenv "HOME") "/.globalrc"))
    (setenv "GTAGSLABEL" "native-pygments"))
  (defun +citer-enable ()
    (interactive)
    (require 'citre-config))
  :config
  (defun +citer-edit-cmd-buf-add-dir-from-godotmod ()
    ;; TODO Add all third part package to ctire command line.
    (interactive))
  (with-eval-after-load 'cc-mode (require 'citre-lang-c))
  (with-eval-after-load 'dired (require 'citre-lang-fileref))
  (with-eval-after-load 'verilog-mode (require 'citre-lang-verilog))
  (define-key citre-peek-keymap (kbd "M-l s") #'citre-peek-save-session)
  (define-key citre-peek-keymap (kbd "M-l h") #'citre-peek-chain-backward)
  (define-key citre-peek-keymap (kbd "M-l l") #'citre-peek-chain-forward)
  (define-key citre-peek-keymap (kbd "M-l j") #'citre-peek-prev-branch)
  (define-key citre-peek-keymap (kbd "M-l k") #'citre-peek-next-branch)
  (define-key citre-peek-keymap (kbd "M-l J") #'citre-peek-move-current-def-up)
  (define-key citre-peek-keymap (kbd "M-l K") #'citre-peek-move-current-def-down))

;;; Lint

(eat-package flymake
  :hook (prog-mode-hook . flymake-mode)
  :init
  (defun sekiro-flymake-mode-line-format ()
    (let* ((counter (string-to-number
                     (nth 1
                          (cadr
                           (flymake--mode-line-counter :error t)))))
           (sekiro-flymake (if (> counter 0)
                               'compilation-error
                             'default)))
      (propertize
       "危"
       'face
       sekiro-flymake))))

(eat-package flymake-flycheck
  :straight t
  :after flymake
  ;; :hook (flymake-mode-hook . enable-flymake-flycheck)
  :init
  (defun enable-flymake-flycheck ()
    (interactive)
    (setq-local flymake-diagnostic-functions
                (append flymake-diagnostic-functions
                        (flymake-flycheck-all-chained-diagnostic-functions))))
  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers
                  (append (default-value 'flycheck-disabled-checkers)
                          '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package)))
    (with-eval-after-load 'go
      (setq flycheck-disabled-checkers
            (append (default-value 'flycheck-disabled-checkers)
                    '(go-gofmt go-golint go-vet go-build go-test go-errcheck))))))

(eat-package flymake-collection
  :straight (flymake-collection :type git :host github :repo "mohkale/flymake-collection"))

;;; Misc

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
  :init
  (setq apheleia-remote-algorithm 'local)
  :config
  (setf (alist-get 'gofmt apheleia-formatters)
        '("goimports")))

(eat-package xscheme)

(eat-package dumb-jump
  :straight t
  :init
  (setq
   dumb-jump-quiet t
   dumb-jump-aggressive t
   dumb-jump-selector 'completing-read))

(eat-package eglot
  :straight t
  :commands eglot-ensure
  :init
  (setq
   eglot-ignored-server-capabilites '(:documentHighlightProvider)

   ;; don't block of LSP connection attempts
   eglot-sync-connect nil

   ;; I will manage these myself
   eglot-stay-out-of '(company flymake)

   ;; make eglot manage file out of project by `xref-find-definitions'
   eglot-extend-to-xref t)

  :config
  ;; Add `eglot-flymake-backend' to `flymake-diagnostic-functions' manually
  ;; So that can run other lints together with `eglot-flymake'
  ;; And make sure that lsp check first run
  (with-eval-after-load 'flymake
    (add-to-list 'flymake-diagnostic-functions 'eglot-flymake-backend))

  ;; keybindings
  (define-key eglot-mode-map (kbd "M-RET") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c h") 'eldoc)
  (define-key eglot-mode-map (kbd "M-'") 'eglot-find-implementation)
  (global-set-key (kbd "<f2>") 'eglot)

  ;; TODO code actions
  (add-to-list 'eglot-server-programs
               '(sql-mode . ("sqls" "-config" "~/.config/sqls/config.yaml")))
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
			   '(rust-mode "rust-analyzer")))

;; TODO auto download ltex-ls into emacs root dir and extract to ltex-ls
(eat-package eglot-ltex
  :straight (eglot-ltex :type git :host github :repo "emacs-languagetool/eglot-ltex")
  :hook
  ;; ((markdown-mode-hook org-mode-hook) . eglot-ltex) NOTE enable manually
  :init
  (defun eglot-ltex ()
    (interactive)
    (setq eglot-languagetool-server-path (expand-file-name "ltex-ls/" user-emacs-directory))
    (require 'eglot-ltex)
    (call-interactively #'eglot)))

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
  :config
  (add-hook 'xref-backend-functions #'lsp-bridge-xref-backend nil t)
  (define-key lsp-bridge-mode-map (kbd "M-?") #'lsp-bridge-find-references)
  (define-key lsp-bridge-mode-map (kbd "M-'") #'lsp-bridge-find-impl)
  (define-key lsp-bridge-mode-map (kbd "C-x 4 .") #'lsp-bridge-find-def-other-window)
  (define-key lsp-bridge-mode-map (kbd "C-c r") #'lsp-bridge-rename))

(require 'init-go)
(require 'init-python)
(require 'init-web)
(require 'init-clojure)

(provide 'init-dev)
