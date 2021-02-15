;;; -*- lexical-binding: t -*-

;; (use-package diff-hl
;;   :hook
;;   ((dired-mode . diff-hl-dired-mode)
;;    (prog-mode . diff-hl-mode)
;;    (conf-mode . diff-hl-mode)))
;;
;; (use-package hl-todo
;;   :config
;;   (global-hl-todo-mode))

(leaf diff-hl
  :straight t
  :hook
  ((dired-mode-hook prog-mode-hook conf-mode-hook) . diff-hl-mode))

(leaf hl-todo
  :straight t
  :config
  (global-hl-todo-mode))

(provide 'init-hl)
