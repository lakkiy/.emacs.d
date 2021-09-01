;;; -*- lexical-binding: t -*-

(eat-package tree-sitter
  :straight t
  :init
  (eat-package tree-sitter-langs :straight t)
  :hook
  ((go-mode-hook
    python-mode-hook
    agda-mode-hook
    c-mode-hook
    c++-mode-hook
    css-mode-hook
    html-mode-hook
    js-mode-hook
    js2-mode-hook
    son-mode-hook
    ruby-mode-hook
    rust-mode-hook
    typescript-mode-hook)
   . tree-sitter-mode)
  (tree-sitter-after-on-hook . tree-sitter-hl-mode)
  :config
  (require 'tree-sitter-langs))

(eat-package citre
  :straight (citre :type git :host github :repo "universal-ctags/citre")
  :init
  (eat-package clue
    :straight (clue :type git :host github :repo "AmaiKinono/clue"))
  (require 'citre-config)
  (setq citre-default-create-tags-file-location 'global-cache
        citre-use-project-root-when-creating-tags t
        citre-prompt-language-for-ctags-command t
        citre-auto-enable-citre-mode '(prog-mode))
  (defun citre-jump+ ()
    "Jump to the definition of the symbol at point.
Fallback to `xref-find-definitions'."
    (interactive)
    (condition-case _
        (citre-jump)
      (error (call-interactively #'xref-find-definitions))))
  (global-set-key (kbd "C-x c j") 'citre-jump+)
  (global-set-key (kbd "C-x c c") 'citre-create-tags-file)
  (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)
  (global-set-key (kbd "C-x c k") 'citre-jump-back)
  (global-set-key (kbd "C-x c p") 'citre-peek)
  (global-set-key (kbd "C-x c a") 'citre-ace-peek)
  (defun company-citre (-command &optional -arg &rest _ignored)
    "Completion backend of Citre.  Execute COMMAND with ARG and IGNORED."
    (interactive (list 'interactive))
    (cl-case -command
      (interactive (company-begin-backend 'company-citre))
      (prefix (and (bound-and-true-p citre-mode)
                   (or (citre-get-symbol) 'stop)))
      (meta (citre-get-property 'signature -arg))
      (annotation (citre-capf--get-annotation -arg))
      (candidates (all-completions -arg (citre-capf--get-collection -arg)))
      (ignore-case (not citre-completion-case-sensitive))))
  :config
  (global-set-key (kbd "C-x c P r") #'citre-peek-restore)
  (global-set-key (kbd "C-x c P l") #'citre-peek-load-session)
  (define-key citre-peek-keymap (kbd "M-l s") #'citre-peek-save-session)
  (define-key citre-peek-keymap (kbd "M-l h") #'citre-peek-chain-backward)
  (define-key citre-peek-keymap (kbd "M-l l") #'citre-peek-chain-forward)
  (define-key citre-peek-keymap (kbd "M-l j") #'citre-peek-prev-branch)
  (define-key citre-peek-keymap (kbd "M-l k") #'citre-peek-next-branch)
  (define-key citre-peek-keymap (kbd "M-l J") #'citre-peek-move-current-def-up)
  (define-key citre-peek-keymap (kbd "M-l K") #'citre-peek-move-current-def-down)
  (with-eval-after-load "company"
    (setq company-backends '((company-capf company-citre :with company-yasnippet :separate)
                             (company-dabbrev-code company-keywords company-files)
                             company-dabbrev))
    ;; Remove duplicate candidate.
    (add-to-list 'company-transformers #'delete-dups))
  ;; The below advice makes Citre come to rescue when enabled xref backends can't find a definition.
  ;; So, when you enable the lsp backend, this tries lsp first, then use Citre.
  (define-advice xref--create-fetcher (:around (-fn &rest -args) fallback)
    (let ((fetcher (apply -fn -args))
          (citre-fetcher
           (let ((xref-backend-functions '(citre-xref-backend t)))
             (apply -fn -args))))
      (lambda ()
        (or (with-demoted-errors "%s, fallback to citre"
              (funcall fetcher))
            (funcall citre-fetcher))))))

(eat-package devdocs
  :straight (devdocs :type git :host github :repo "astoff/devdocs.el")
  :init
  (global-set-key (kbd "C-c b") 'devdocs-lookup))

(eat-package flymake
  :commands flymake-mode
  :after go-mode
  :config
  (define-key flymake-mode-map (kbd "C-c C-b") 'flymake-show-diagnostics-buffer)
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

(eat-package flycheck
  :straight t
  ;; :hook (prog-mode-hook . flycheck-mode)
  :init
  (setq flycheck-temp-prefix ".flycheck"
        flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-emacs-lisp-load-path 'inherit
        flycheck-indication-mode (if (display-graphic-p)
                                     'right-fringe
                                   'right-margin))
  :config
  (define-key flycheck-mode-map (kbd "C-c C-b") 'flycheck-list-errors)
  (define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error))

(eat-package eglot
  :straight t
  :commands
  eglot-ensure
  :hook (go-mode-hook . eglot-ensure)
  :init
  (setq eglot-stay-out-of nil
        eglot-ignored-server-capabilites '(:documentHighlightProvider))
  ;; I will manage `company-capf' myself
  (add-to-list 'eglot-stay-out-of 'company)
  ;; auto expand function param for golang
  ;; (setq-default eglot-workspace-configuration
  ;;               '((gopls
  ;;                  (usePlaceholders . t))))
  :config
  ;; TODO language server configuration
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
			   '(rust-mode "rust-analyzer")))

(require 'init-go)
(require 'init-python)
(require 'init-c)
(require 'init-web)

(provide 'init-dev)
