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
  (global-set-key (kbd "C-x c j") 'citre-jump)
  (global-set-key (kbd "C-x c J") 'citre-jump-back)
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
    (global-set-key (kbd "C-x c r") 'citre-jump-to-reference)
    (global-set-key (kbd "C-x c P") 'citre-ace-peek-references)
    (with-eval-after-load 'citre-peek
      (define-key citre-peek-keymap (kbd "M-l r")
                  'citre-peek-through-references))
    :config
    (setenv "GTAGSOBJDIRPREFIX" (concat (getenv "HOME") "/.cache/gtags"))
    (setenv "GTAGSCONF" (concat (getenv "HOME") "/.globalrc"))
    (setenv "GTAGSLABEL" "native-pygments")))

(eat-package eldoc-box
  :straight t
  :hook (eglot-managed-mode-hook . eldoc-box-hover-mode))

(eat-package eglot
  :straight t
  :commands eglot-ensure
  :init
  (setq eglot-events-buffer-size 0
        eglot-sync-connect nil       ;; don't block of LSP connection attempts
        eglot-extend-to-xref t       ;; make eglot manage file out of project by `xref-find-definitions'
        eglot-ignored-server-capabilites '(:documentHighlightProvider :documentFormattingProvider :documentRangeFormattingProvider))
  (setq-default eglot-workspace-configuration
                '((gopls
                   (usePlaceholders . t))))
  :config
  ;; keybindings
  (define-key eglot-mode-map (kbd "M-RET") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "M-'") 'eglot-find-implementation)
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
			   '(rust-mode "rust-analyzer"))
  ;; NOTE deno
  (defclass eglot-deno (eglot-lsp-server) ()
    :documentation "A custom class for deno lsp.")
  (cl-defmethod eglot-initialization-options ((server eglot-deno))
    "Passes through required deno initialization options"
    (list :enable t
          :lint t))
  (add-to-list 'eglot-server-programs '((js-mode typescript-mode) . (eglot-deno "deno" "lsp")))
  ;; TODO code actions
  (add-to-list 'eglot-server-programs
               '(sql-mode . ("sqls" "-config" "~/.config/sqls/config.yaml"))))

;; this need pip install epc, orjson
(eat-package lsp-bridge
  :straight (lsp-bridge
             :type git
             :host github
             :repo "manateelazycat/lsp-bridge"
             :files ("*"))
  :commands lsp-bridge-mode global-lsp-bridge-mode
  :init
  (setq acm-enable-doc nil
        acm-enable-search-words nil
        lsp-bridge-enable-search-words nil)

  (defun eat/lsp-bridge-mode-setup ()
    "My setup for lsp-bridge.

Disable `corfu-mode'.
When expand snippet, try complete if there's acm cond, or run `yas-next-field-or-maybe-expand'."
    (interactive)
    (ignore-errors
      (corfu-mode -1))
    (with-eval-after-load 'yasnippet
      (define-key yas-keymap (kbd "<tab>") 'acm-complete-or-expand-yas-snippet)
      (define-key yas-keymap (kbd "TAB") 'acm-complete-or-expand-yas-snippet)))
  :config
  (add-hook 'lsp-bridge-mode-hook #'eat/lsp-bridge-mode-setup)
  ;; keybindings
  (define-key lsp-bridge-mode-map (kbd "M-.") #'lsp-bridge-find-def)
  (define-key lsp-bridge-mode-map (kbd "C-x 4 .") #'lsp-bridge-find-def-other-window)
  (define-key lsp-bridge-mode-map (kbd "M-,") #'lsp-bridge-return-from-def)
  (define-key lsp-bridge-mode-map (kbd "M-?") #'lsp-bridge-find-references)
  (define-key lsp-bridge-mode-map (kbd "M-'") #'lsp-bridge-find-impl)
  (define-key lsp-bridge-mode-map (kbd "C-c r") #'lsp-bridge-rename)
  (define-key lsp-bridge-mode-map (kbd "C-c <") #'lsp-bridge-jump-to-prev-diagnostic)
  (define-key lsp-bridge-mode-map (kbd "C-c >") #'lsp-bridge-jump-to-next-diagnostic)
  ;; FIXME p will say search failed " ", it should act like n, reach last file
  (define-key lsp-bridge-ref-mode-map (kbd "j") nil)
  (define-key lsp-bridge-ref-mode-map (kbd "k") nil)
  (define-key lsp-bridge-ref-mode-map (kbd "h") nil)
  (define-key lsp-bridge-ref-mode-map (kbd "l") nil)
  (define-key lsp-bridge-ref-mode-map (kbd "p") 'lsp-bridge-ref-jump-prev-file)
  (define-key lsp-bridge-ref-mode-map (kbd "h") 'lsp-bridge-ref-jump-prev-keyword)
  (define-key lsp-bridge-ref-mode-map (kbd "t") 'lsp-bridge-ref-jump-next-keyword)
  (define-key lsp-bridge-ref-mode-map (kbd "n") 'lsp-bridge-ref-jump-next-file))

(eat-package acm-terminal
  :init
  (unless (display-graphic-p)
    (with-eval-after-load 'acm
      (require 'acm-terminal))))

(defun eat/lsp-reconnect ()
  (interactive)
  (if (bound-and-true-p lsp-bridge-mode)
      (lsp-bridge-restart-process)
    (eglot-reconnect)))
(global-set-key [f10] #'eat/lsp-reconnect)

(require 'init-go)
(require 'init-clojure)

(provide 'init-dev)
