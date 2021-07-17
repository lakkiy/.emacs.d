;;; -*- lexical-binding: t -*-

(eat-package whitespace
  :hook
  ((prog-mode-hook conf-mode-hook) . whitespace-mode)
  :init
  (setq whitespace-style '(face trailing)))

(eat-package hideshow
  :doc "fold and display code/comment blocks"
  :hook (prog-mode-hook . hs-minor-mode))

(eat-package xref
  :init

  (global-unset-key (kbd "C-<down-mouse-1>"))
  (global-set-key (kbd "C-<mouse-1>") #'xref-find-definitions-at-mouse)
  ;; Xref no prompt
  (setq xref-prompt-for-identifier nil))

(eat-package pluse
  :init
  (defun pulse-region (beg end &rest _)
    "Pulse the current region."
    (pulse-momentary-highlight-region beg end))
  (defun pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))
  (defun recenter-and-pulse (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (pulse-line))
  (advice-add #'xref-find-definitions :after #'recenter-and-pulse)
  (advice-add #'xref-find-definitions-at-mouse :after #'recenter-and-pulse)
  (advice-add #'xref-pop-marker-stack :after #'recenter-and-pulse)
  :hook
  ((bookmark-after-jump-hook imenu-after-jump-hook) . recenter-and-pulse))

(eat-package dumb-jump
  :straight t
  :hook (dump-jump-after-jump-hook . recenter-and-pulse)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate t)
  (setq dumb-jump-quiet t
        dumb-jump-aggressive t
        dumb-jump-prefer-search 'rg
        dumb-jump-selector 'completing-read
        dumb-jump-disable-obsolate-warning t)

  (global-set-key (kbd "M-g J") 'dumb-jump-go-other-window)
  (global-set-key (kbd "M-g j") 'dumb-jump-go))

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
  :commands
  citre-mode
  citre-update-this-tags-file
  citre-jump-back
  :init
  (setq citre-default-create-tags-file-location 'global-cache
        citre-use-project-root-when-creating-tags t
        citre-prompt-language-for-ctags-command t)
  (global-set-key (kbd "C-x c x") 'citre-update-this-tags-file)
  (global-set-key (kbd "C-x c j") 'citre-jump)
  (global-set-key (kbd "C-x c J") 'citre-jump-back)
  (global-set-key (kbd "C-x c p") 'citre-peek)
  (global-set-key (kbd "C-x c P") 'citre-ace-peek)
  (defun citre-jump+ ()
    (interactive)
    (condition-case _
        (citre-jump)
      (error (call-interactively #'xref-find-definitions))))
  :config
  (with-eval-after-load 'c-mode
    (require 'citre-lang-c)
    (add-hook 'c-mode-hook #'citre-auto-enable-citre-mode))
  (with-eval-after-load 'dired (require 'citre-lang-fileref)))

(eat-package devdocs
  :straight (devdocs :type git :host github :repo "astoff/devdocs.el")
  :init
  (global-set-key (kbd "C-c b") 'devdocs-lookup))

(eat-package docstr
  :straight t
;; FIXME not work with meow in go mode
  :hook (prog-mode-hook . (lambda () (docstr-mode 1))))

(eat-package flymake
  :commands flymake-mode
  :after prog-mode
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
        flycheck-indication-mode 'right-fringe)
  :config
  (define-key flycheck-mode-map (kbd "C-c C-b") 'flycheck-list-errors)
  (define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error))

(eat-package eglot
  :straight t
  :commands
  eglot-ensure
  eglot
  :hook (go-mode-hook . eglot-ensure)
  :init
  (setq eglot-stay-out-of nil
        eglot-ignored-server-capabilites '(:documentHighlightProvider))
  ;; auto expand function param for golang
  ;; (setq-default eglot-workspace-configuration
  ;;               '((gopls
  ;;                  (usePlaceholders . t))))
  :config
  (add-to-list 'eglot-server-programs
			   '(rust-mode "rust-analyzer")))

(require 'init-go)
(require 'init-python)
(require 'init-c)
(require 'init-web)

(provide 'init-dev)
