;;; -*- lexical-binding: t -*-

(eat-package magit
  :straight t
  :commands magit)

(eat-package magit-todos
  :straight t
  :init
  (setq magit-todos-nice (if (executable-find "nice") t nil))
  (with-eval-after-load "magit"
    (let ((inhibit-message t))
      (magit-todos-mode 1)))
  :config
  (transient-append-suffix 'magit-status-jump '(0 0 -1)
    '("T " "Todos" magit-todos-jump-to-todos)))

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

(eat-package smerge-mode
  :commands smerge-mode
  :hook
  (find-file-hook . (lambda ()
                      (save-excursion
                        (goto-char (point-min))
                        (when (re-search-forward "^<<<<<<< " nil t)
                          (smerge-mode 1)))))
  :config
  (define-key smerge-mode-map (kbd "C-c m") #'smerge-mode-hydra/body))

(provide 'init-git)
