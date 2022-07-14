;;; -*- lexical-binding: t -*-

(straight-use-package 'color-theme-sanityinc-tomorrow)
(straight-use-package 'spacemacs-theme)
(straight-use-package 'kaolin-themes)
(straight-use-package 'stimmung-themes)
(straight-use-package '(notink-theme :type git :host github :repo "MetroWind/notink-theme"))


;;; Theme
;; `doom-themes'
(with-eval-after-load 'doom-themes
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

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


;;; Mics
(eat-package info-variable-pitch
  :straight (info-variable-pitch
             :type git
             :host github
             :repo "kisaragi-hiu/info-variable-pitch")
  :hook (Info-mode-hook . #'info-variable-pitch-mode))

;;; init-ui.el ends here
(provide 'init-ui)
