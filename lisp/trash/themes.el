;; `the-matrix-theme'
(straight-use-package 'the-matrix-theme)
;; `acme-theme'
(straight-use-package 'acme-theme)

;; `doom-themes'
(straight-use-package 'doom-themes)

(with-eval-after-load 'doom-themes
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; `atom-one-dark-theme'
(straight-use-package 'atom-one-dark-theme)
