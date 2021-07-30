;;; -*- lexical-binding: t -*-

(eat-package magit
  :straight t
  :commands magit)

(eat-package diff-hl
  :straight t
  :commands diff-hl-mode
  :hook
  ((prog-mode-hook conf-mode-hook) . diff-hl-mode)
  (dired-mode-hook . diff-hl-dired-mode))

(eat-package hl-todo
  :straight t
  :hook
  ((dired-mode-hook prog-mode-hook conf-mode-hook) . hl-todo-mode))

(provide 'init-git)
