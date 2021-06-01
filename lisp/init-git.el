;;; -*- lexical-binding: t -*-

(straight-use-package 'magit)
(straight-use-package 'magit-todos)
(straight-use-package 'hl-todo)
(straight-use-package 'diff-hl)

(+pdump-packages 'magit
                 'magit-todos
                 'diff-hl
                 'hl-todo)

;; magit
(autoload #'magit "magit" nil t)

(with-eval-after-load "magit"
  ;;; magit-todos
  (setq magit-todos-nice (if (executable-find "nice") t nil))
  (let ((inhibit-message t)) (magit-todos-mode 1))
  (with-eval-after-load "magit-todos"
    (transient-append-suffix 'magit-status-jump '(0 0 -1)
      '("T " "Todos" magit-todos-jump-to-todos))))

;; diff-hl
(autoload #'diff-hl-mode "diff-hl")

(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'prog-mode-hook 'diff-hl-mode)
(add-hook 'conf-mode-hook 'diff-hl-mode)

;; hl-todo
(add-hook 'dired-mode-hook 'hl-todo-mode)
(add-hook 'prog-mode-hook 'hl-todo-mode)
(add-hook 'conf-mode-hook 'hl-todo-mode)

(provide 'init-git)
