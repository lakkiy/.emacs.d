;;; -*- lexical-binding: t -*-

(leaf diff-hl
  :straight t
  :hook
  ((dired-mode-hook prog-mode-hook conf-mode-hook) . diff-hl-mode))

(leaf hl-todo
  :straight t
  :global-minor-mode global-hl-todo-mode)

(provide 'init-hl)
