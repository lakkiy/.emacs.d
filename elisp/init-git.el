;;; -*- lexical-binding: t -*-

(use-package magit
  :commands (magit))

(use-package dired-git-info
  :bind
  (:map dired-mode-map
        ("v" . dired-git-info-mode))
  :custom
  (dgi-auto-hide-details-p nil))

(provide 'init-git)
