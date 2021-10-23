;;; -*- lexical-binding: t -*-

(eat-package all-the-icons
  :straight t)

(eat-package all-the-icons-ibuffer
  :after ibuffer
  :straight t
  :hook (ibuffer-mode-hook . all-the-icons-ibuffer-mode))

(eat-package all-the-icons-dired
  :straight t
  :hook (dired-mode-hook . all-the-icons-dired-mode))

(eat-package treemacs
  :init
  (setq treemacs-no-png-images nil))

(eat-package all-the-icons-completion
  :straight (all-the-icons-completion :type git :host github :repo "iyefrat/all-the-icons-completion")
  :init
  (all-the-icons-completion-mode)
  :config
  (with-eval-after-load "marginalia"
    (add-hook 'marginalia-mode-hook #'all-the-icons-marginalia-setup)))

(provide 'init-icons)
