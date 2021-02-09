;;; -*- lexical-binding: t -*-

(use-package yasnippet
  :config
  (let ((inhibit-message t)) (yas-reload-all))
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package yasnippet-snippets)

(use-package company
  :bind (("M-/" . company-complete)
         ("C-M-i" . company-complete)
         :map company-mode-map
         ("<backtab>" . company-yasnippet)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("<tab>" . company-complete-common-or-cycle)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.2)
  (company-tooltip-idle-delay 0.1)
  (company-tooltip-limit 10)
  (company-tooltip-align-annotations t)
  (company-tooltip-width-grow-only t)
  (company-dabbrev-downcase nil)
  (company-global-modes '(not org-mode dired-mode dired-sidebar-mode))
  (company-require-match nil))

(use-package ivy
  :init
  (setq ivy-initial-inputs-alist nil)
  (ivy-mode 1)
  :custom
  (ivy-use-selectable-prompt t))

(use-package counsel
  :bind
  ("C-s" . swiper)
  ("C-c z" . counsel-fzf)
  ("C-c r" . counsel-rg)
  :init
  (counsel-mode 1))

(use-package ivy-xref
  :straight
  (ivy-xref :type git
            :host github
            :repo "alexmurray/ivy-xref")
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(provide 'init-completion)
