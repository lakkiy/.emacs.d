;;; -*- lexical-binding: t -*-

(eat-package magit
  :straight t
  :commands magit)

(eat-package magit-todos
  :straight t
  :after magit
  :config
  (setq magit-todos-nice (if (executable-find "nice") t nil))
  (let ((inhibit-message t)) (magit-todos-mode 1))
  (transient-append-suffix 'magit-status-jump '(0 0 -1)
    '("T " "Todos" magit-todos-jump-to-todos)))

(eat-package diff-hl
  :straight t
  :commands diff-hl-mode
  :hook
  ((prog-mode-mode conf-mode-hook) . diff-hl-mode)
  (dired-mode-hook . diff-hl-dired-mode))

(eat-package hl-todo
  :straight t
  :hook
  ((dired-mode-hook prog-mode-hook conf-mode-hook) . hl-todo-mode))

(provide 'init-git)
