;;; -*- lexical-binding: t -*-

(with-eval-after-load 'treemacs
  (setq treemacs-no-png-images nil)
  (require 'treemacs-all-the-icons)
  (treemacs-load-theme "all-the-icons"))

(eat-package all-the-icons-ibuffer
  :after ibuffer
  :straight t
  :hook (ibuffer-mode-hook . all-the-icons-ibuffer-mode))

(eat-package all-the-icons-dired
  :straight t
  :hook (dired-mode-hook . all-the-icons-dired-mode))

(eat-package all-the-icons-completion
  :straight (all-the-icons-completion
             :type git
             :host github
             :repo "iyefrat/all-the-icons-completion")
  :hook (after-init-hook . all-the-icons-completion-mode)
  :config
  ;;(with-eval-after-load "marginalia"
  ;;  (add-hook 'marginalia-mode-hook #'all-the-icons-marginalia-setup))
  )

(provide 'init-icons)
