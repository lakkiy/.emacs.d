;;; -*- lexical-binding: t -*-

(eat-package recentf
  :hook (after-init-mode . recentf-mode)
  :init
  (global-set-key (kbd "C-x C-r") #'recentf-open-files)
  (setq recentf-max-saved-items 300
        recentf-exclude
        '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
          "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
          "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
          (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))

;; (eat-package display-line-numbers
;;   :hook ((prog-mode-hook conf-mode-hook) . display-line-numbers-mode))

(eat-package subword
  :doc "handling capitalized subwords in a nomenclature"
  :hook (prog-mode-hook . subword-mode))

(eat-package simple
  :hook
  (before-save-hook . delete-trailing-whitespace))

(eat-package tab-bar
  :init
  (setq tab-bar-show nil
        tab-bar-new-tab-choice "*scratch*"))

(eat-package so-long
  :hook (after-init-hook . global-so-long-mode))

(eat-package repeat
  :doc "repeat the previous command"
  ;; HACK custom
  :init
  (setq repeat-mode t
        repeat-keep-prefix t
        repeat-exit-timeout 3
        repeat-exit-key (kbd "RET")))

(eat-package hl-line
  :doc "Highlight current line, only enable in GUI"
  ;; HACK when (display-graphic-p)
  :hook
  ((prog-mode-hook conf-mode-hook) . hl-line-mode))

(eat-package autorevert
  :doc "revert buffers when files on disk change"
  :hook (after-init-hook . global-auto-revert-mode))

(eat-package elec-pair
  :doc "Automatic parenthesis pairing"
  :hook (prog-mode-hook . electric-pair-mode)
  :init
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(eat-package saveplace
  :hook (after-init-hook . save-place-mode))

(eat-package paren
  :hook (prog-mode-hook . show-paren-mode)
  :init
  (setq show-paren-when-point-in-periphery t
        show-paren-when-point-inside-paren t))

(eat-package tramp
  :doc "transparent remote access"
  :init
  ;; Always use file cache when using tramp
  (setq remote-file-name-inhibit-cache nil
        ;; C-x C-f /ssh:
        tramp-default-method "ssh"))

(eat-package eldoc
  :init
  (setq eldoc-idle-delay 2))

(eat-package whitespace
  :hook
  ((prog-mode-hook conf-mode-hook) . whitespace-mode)
  :init
  (setq whitespace-style '(face trailing)))

(eat-package hideshow
  :doc "fold and display code/comment blocks"
  :hook (prog-mode-hook . hs-minor-mode))

(eat-package xref
  :init
  (global-unset-key (kbd "C-<down-mouse-1>"))
  (global-set-key (kbd "C-<mouse-1>") #'xref-find-definitions-at-mouse)
  ;; Xref no prompt
  (setq xref-prompt-for-identifier nil)
  (when (>= emacs-major-version 28)
    (setq xref-search-program 'ripgrep)
    (setq xref-show-xrefs-function #'xref-show-definitions-completing-read)
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read)))

(eat-package pluse
  :init
  (defun pulse-region (beg end &rest _)
    "Pulse the current region."
    (pulse-momentary-highlight-region beg end))
  (defun pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))
  (defun recenter-and-pulse (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (pulse-line))
  (advice-add #'xref-find-definitions :after #'recenter-and-pulse)
  (advice-add #'xref-find-definitions-at-mouse :after #'recenter-and-pulse)
  (advice-add #'xref-pop-marker-stack :after #'recenter-and-pulse)
  :hook
  ((bookmark-after-jump-hook imenu-after-jump-hook) . recenter-and-pulse))

;; Undo/redo changes to Emacs' window layout
(eat-package winner
  :hook (after-init-hook . winner-mode)
  :config
  (setq winner-dont-bind-my-keys t))

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

(eat-package dired
  :init
  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t))

(eat-package ibuffer
  :init
  (fset 'list-buffers 'ibuffer))

(eat-package ediff
  :init
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-highlight-all-diffs t
        ediff-split-window-function 'split-window-horizontally
        ediff-merge-split-window-function 'split-window-horizontally))

;; built-in since emacs 28
(eat-package modus-themes
  :init
  (setq
   ;; I will padded myself.
   modus-themes-mode-line '(accented borderless)))

(provide 'init-builtin)
