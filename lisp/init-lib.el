;;; -*- lexical-binding: t -*-

(install-package 'popon)
(install-package 'fullframe)
(install-package 'hide-mode-line)

;; run `nerd-icons-install-fonts'
;; ttf-nerd-fonts-symbols-1000-em-mono
(install-package 'nerd-icons)

(install-package 'elpa-mirror)
(setq elpamr-default-output-directory "~/Sync/myelpa")

(install-package 'visual-fill-column)
(add-hook 'visual-fill-column-mode-hook #'visual-line-mode)
(setq visual-fill-column-center-text t)

(install-package 'valign)
(setq valign-fancy-bar t)

;;; init-lib.el ends here
