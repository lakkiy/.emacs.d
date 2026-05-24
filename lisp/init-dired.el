;;; init-dired.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;; NOETS: for subtree, press i in dired.

;;; dirvish
(install-package 'dirvish)

(add-hook 'after-init-hook #'dirvish-override-dired-mode)

(setq dirvish-attributes '(vc-state subtree-state nerd-icons)
      dirvish-header-line-height 20
      dirvish-mode-line-height 20)

(keymap-global-set "<f1>" #'dirvish-side)

(with-eval-after-load 'dirvish
  (dirvish-side-follow-mode)
  (define-key dirvish-mode-map (kbd "TAB") #'dirvish-subtree-toggle)
  (define-key dirvish-mode-map (kbd "<tab>") #'dirvish-subtree-toggle)
  (define-key dirvish-mode-map (kbd "a") #'dirvish-quick-access)
  (define-key dirvish-mode-map (kbd "f") #'dirvish-file-info-menu)
  (define-key dirvish-mode-map (kbd "y") #'dirvish-yank-menu)
  (define-key dirvish-mode-map (kbd "N") #'dirvish-narrow)
  (define-key dirvish-mode-map (kbd "H") #'dirvish-history-jump)
  (define-key dirvish-mode-map (kbd "s") #'dirvish-quicksort)
  (define-key dirvish-mode-map (kbd "v") #'dirvish-vc-menu)
  (define-key dirvish-mode-map (kbd "M-f") #'dirvish-history-go-forward)
  (define-key dirvish-mode-map (kbd "M-b") #'dirvish-history-go-backward)
  (define-key dirvish-mode-map (kbd "M-l") #'dirvish-ls-switches)
  (define-key dirvish-mode-map (kbd "M-m") #'dirvish-mark-menu)
  (define-key dirvish-mode-map (kbd "M-t") #'dirvish-layout-toggle)
  (define-key dirvish-mode-map (kbd "M-s") #'dirvish-setup-menu)
  (define-key dirvish-mode-map (kbd "M-e") #'dirvish-emerge-menu)
  (define-key dirvish-mode-map (kbd "M-j") #'dirvish-fd-jump)
  (define-key dirvish-mode-map (kbd "<mouse-1>") #'dirvish-subtree-toggle-or-open)
  (define-key dirvish-mode-map (kbd "<mouse-2>") #'dired-mouse-find-file-other-window)
  (define-key dirvish-mode-map (kbd "<mouse-3>") #'dired-mouse-find-file)

  (define-key dirvish-mode-map (kbd "r") #'dirvish-rsync-switches-menu))

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
      dired-clean-confirm-killing-deleted-buffers nil)

(setq dired-omit-verbose nil)
(setq dired-omit-files (rx string-start
                           (or ".DS_Store"
                               ".cache"
                               ".vscode"
                               "__pycache__"
                               ".ccls-cache" ".clangd")
                           string-end))

(add-hook 'dired-mode-hook #'dired-omit-mode)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(with-eval-after-load 'dired
  (setq dired-listing-switches
        "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
  (keymap-set dired-mode-map "C-c C-p" #'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd "h") #'dired-up-directory)
  (define-key dired-mode-map [mouse-2] #'dired-find-file))

;;; init-dired ends here
