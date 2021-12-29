;;; -*- lexical-binding: t -*-

(eat-package git-modes
  :straight t
  :init
  (add-to-list 'auto-mode-alist
               (cons "/.dockerignore\\'" 'gitignore-mode)))

(eat-package magit
  :straight t
  :hook (git-commit-setup-hook . git-commit-turn-on-flyspell)
  :commands magit)

(eat-package magit-delta
  :straight t
  :init
  (when (executable-find "delta")
    (add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))))

(eat-package diff-hl
  :straight t
  :commands diff-hl-mode
  :hook
  ((prog-mode-hook conf-mode-hook) . diff-hl-mode)
  (dired-mode-hook . diff-hl-dired-mode)
  :init
  (setq diff-hl-draw-borders nil)
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)

  (unless (display-graphic-p)
    ;; Fall back to the display margin since the fringe is unavailable in tty
    (diff-hl-margin-mode 1)
    ;; Avoid restoring `diff-hl-margin-mode'
    (with-eval-after-load 'desktop
      (add-to-list 'desktop-minor-mode-table
                   '(diff-hl-margin-mode nil)))))

(eat-package hl-todo
  :straight t
  :hook
  ((dired-mode-hook prog-mode-hook conf-mode-hook) . hl-todo-mode))

(provide 'init-git)
