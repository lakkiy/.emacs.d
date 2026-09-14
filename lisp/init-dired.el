;;; init-dired.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Dired
(setq mouse-drag-and-drop-region t
      mouse-drag-and-drop-region-cross-program t)

(setq dired-dwim-target t
      dired-vc-rename-file t
      dired-mouse-drag-files t
      dired-auto-revert-buffer t
      dired-recursive-copies 'always
      dired-create-destination-dirs 'ask
      dired-deletion-confirmer 'y-or-n-p
      dired-kill-when-opening-new-dired-buffer t
      dired-clean-confirm-killing-deleted-buffers nil
      dired-listing-switches
      "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group"
      dired-omit-verbose nil
      dired-omit-files (rx string-start
                           (or ".DS_Store"
                               ".cache"
                               ".vscode"
                               "__pycache__ "
                               ".ccls-cache" ".clangd")
                           string-end))

(add-hook 'dired-mode-hook #'dired-omit-mode)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(with-eval-after-load 'dired
  (keymap-set dired-mode-map "C-c C-p" #'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd "h") #'dired-up-directory))

;;; subtree
(install-package 'dired-subtree)

(setq dired-subtree-use-backgrounds nil)

(with-eval-after-load 'dired
  (keymap-set dired-mode-map "TAB" #'dired-subtree-toggle)
  (keymap-set dired-mode-map "<backtab>" #'dired-subtree-toggle))

;;; sidebar
(install-package 'dired-sidebar)

(setq dired-sidebar-should-follow-file t
      dired-sidebar-theme 'nerd-icons
      dired-sidebar-subtree-line-prefix "  ")

(keymap-global-set "C-x C-n" #'dired-sidebar-toggle-sidebar)

;;; icon
(install-package 'nerd-icons-dired)
(add-hook 'dired-mode-hook #'nerd-icons-dired-mode)

(defun my/dired-subtree-refresh-nerd-icons (&rest _)
  "在 dired-subtree 展开/收起后刷新 nerd-icons-dired 的图标。"
  (when (bound-and-true-p nerd-icons-dired-mode)
    (nerd-icons-dired--refresh)))

(with-eval-after-load 'dired-subtree
  (add-hook 'dired-subtree-after-insert-hook #'my/dired-subtree-refresh-nerd-icons)
  (add-hook 'dired-subtree-after-remove-hook #'my/dired-subtree-refresh-nerd-icons))

;;; init-dired ends here
