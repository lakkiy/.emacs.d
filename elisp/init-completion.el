;;; -*- lexical-binding: t -*-

(use-package yasnippet)

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
  (company-tooltip-limit 10)
  (company-tooltip-align-annotations t)
  (company-tooltip-width-grow-only t)
  (company-dabbrev-downcase nil)
  (company-global-modes '(not org-mode dired-mode dired-sidebar-mode))
  (company-require-match nil))

(use-package ivy
  :init
  (setq ivy-initial-inputs-alist nil)
  (ivy-mode 1))

(use-package counsel
  :bind
  ("C-s" . swiper)
  ("C-c z" . counsel-fzf)
  ("C-c r" . counsel-rg)
  :init
  (counsel-mode 1))

(provide 'init-completion)
