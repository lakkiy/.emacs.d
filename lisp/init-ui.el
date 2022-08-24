;;; -*- lexical-binding: t -*-

(straight-use-package 'spacemacs-theme)
(straight-use-package 'kaolin-themes)
(straight-use-package '(notink-theme :type git :host github :repo "MetroWind/notink-theme"))
(straight-use-package '(ef-themes :type git :host github :repo "protesilaos/ef-themes"))
(straight-use-package '(matrix-emacs-theme :type git :host github :repo "monkeyjunglejuice/matrix-emacs-theme"))
(straight-use-package 'catppuccin-theme)


;;; Theme
;; `spacemacs-theme'
(setq
 spacemacs-theme-comment-italic t
 spacemacs-theme-keyword-italic t
 spacemacs-theme-org-agenda-height t
 spacemacs-theme-org-bold t
 spacemacs-theme-org-height t
 spacemacs-theme-org-highlight t
 spacemacs-theme-org-priority-bold t
 spacemacs-theme-org-bold t
 spacemacs-theme-underline-parens t)

;; `kaolin-themes'
(setq
 kaolin-themes-underline-wave nil
 kaolin-themes-modeline-border nil
 kaolin-themes-modeline-padded 4)

(with-eval-after-load 'kaolin-themes
  (with-eval-after-load 'treemacs
    (with-eval-after-load 'all-the-icons
      (kaolin-treemacs-theme))))


;;; Mode-line

(eat-package mode-line-bell
  :straight t
  :hook (after-init-hook . mode-line-bell-mode))

(eat-package minions
  :straight t
  :hook (after-init-hook . minions-mode))

;;; icon
(when (and eat/enable-icon (display-graphic-p))
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
    :hook (after-init-hook . (lambda ()
                               (all-the-icons-completion-mode)))
    :config
    (with-eval-after-load 'marginalia
      ;; FIXME hook is nil
      (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))))

;;; init-ui.el ends here
(provide 'init-ui)
