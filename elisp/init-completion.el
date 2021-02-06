;;; -*- lexical-binding: t -*-

(use-package yasnippet)

(use-package company
  :config
  (global-company-mode))

(use-package ivy
  :init
  (setq ivy-count-format "")
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
