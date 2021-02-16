;;; -*- lexical-binding: t -*-

(leaf magit
  :straight t
  :commands magit)

(leaf dired-git-info
  :straight t
  :bind ((:dired-mode-map
          ("v" . dired-git-info-mode)))
  :custom
  (dgi-auto-hide-details-p . nil))

(provide 'init-git)
