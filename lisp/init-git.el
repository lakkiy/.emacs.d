;;; -*- lexical-binding: t -*-

(straight-use-package 'magit)

(autoload #'magit "magit")



(straight-use-package '(gitmoji :type git :host github :repo "Tiv0w/gitmoji"))

(setq
 gitmoji--insert-utf8-emoji t
 gitmoji--display-utf8-emoji t)

(with-eval-after-load "magit"
  (require 'gitmoji))



(straight-use-package 'dired-git-info)

(setq
 dgi-auto-hide-details-p nil)

(autoload #'dired-git-info "dired-git-info")

(with-eval-after-load "dired-git-info"
  (define-key dired-mode-map (kbd "v") 'dired-git-info-mode))



(straight-use-package 'diff-hl)

(autoload #'diff-hl-mode "diff-hl")

(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'prog-mode-hook 'diff-hl-mode)
(add-hook 'conf-mode-hook 'diff-hl-mode)



(straight-use-package 'hl-todo)

(add-hook 'dired-mode-hook 'hl-todo-mode)
(add-hook 'prog-mode-hook 'hl-todo-mode)
(add-hook 'conf-mode-hook 'hl-todo-mode)



(provide 'init-git)
