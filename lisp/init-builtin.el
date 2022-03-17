;;; -*- lexical-binding: t -*-

(eat-package recentf
  :hook (after-init-hook . recentf-mode)
  :init
  (setq-default
   recentf-max-saved-items 1000)
  (global-set-key (kbd "C-x C-r") #'recentf-open-files))

(eat-package minibuffer
  :init
  (setq
   ;; `selectrum', `vertico' and `icomplete' will honoring
   ;; completion-styles '(basic partial-completion substring flex)
   ;; completion-category-overrides '((buffer (styles . (flex))))
   completion-cycle-threshold t
   minibuffer-depth-indicate-mode t
   minibuffer-eldef-shorten-default t
   minibuffer-electric-default-mode t))

(eat-package display-line-numbers
  ;; :hook (prog-mode-hook . display-line-numbers-mode)
  :init
  (setq-default
   display-line-numbers-width 3))

(eat-package hippie-exp
  :init
  (global-set-key (kbd "M-/") 'hippie-expand)

  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill)))

(eat-package subword
  :hook (prog-mode-hook . subword-mode))

(eat-package simple
  :hook (before-save-hook . delete-trailing-whitespace)
  :init
  (setq visual-line-fringe-indicators '(nil right-curly-arrow)
        ;; List only applicable commands.
        read-extended-command-predicate #'command-completion-default-include-p))

(eat-package so-long
  :hook (after-init-hook . global-so-long-mode))

(eat-package repeat
  :init
  (setq
   repeat-mode t
   repeat-keep-prefix t
   repeat-exit-timeout 3
   repeat-exit-key (kbd "RET")))

(eat-package hl-line
  :hook
  ((prog-mode-hook conf-mode-hook) . hl-line-mode)
  :init
  (setq-default hl-line-sticky-flag nil))

(eat-package autorevert
  :hook (after-init-hook . global-auto-revert-mode))

(eat-package elec-pair
  :hook (prog-mode-hook . electric-pair-mode)
  :init
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(eat-package saveplace ;; TODO
  :hook (after-init-hook . save-place-mode))

(eat-package paren
  :hook (prog-mode-hook . show-paren-mode)
  :init
  (setq
   show-paren-when-point-in-periphery t
   show-paren-when-point-inside-paren t
   ;; NOTE emacs 29
   show-paren-context-when-offscreen t))

(eat-package tramp
  :init
  (setq
   ;; Always use file cache when using tramp
   remote-file-name-inhibit-cache nil
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
  :hook (prog-mode-hook hs-minor-mode)
  :init
  (setq +hs-folding-fringe-indicators t)

  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap '+hs-folding-fringe-marker
      (vector #b00000000
              #b00000000
              #b00000000
              #b11000011
              #b11100111
              #b01111110
              #b00111100
              #b00011000)))

  (defface +hs-folding-fringe-face
    '((t (:inherit 'font-lock-comment-face
                   :box (:line-width 1 :style released-button))))
    "Face for folding bitmaps appearing on the fringe.")

  (defface +hs-folding-face
    '((t (:inherit 'font-lock-comment-face :box t)))
    "Face for the folded region indicator.")

  (defun +hs-display-code-line-counts (ov)
    "Display a folded region indicator with the number of folded
      lines.

    Meant to be used as `hs-set-up-overlay'."
    (let* ((marker-string "*fringe-dummy*")
           (marker-length (length marker-string)))
      (cond
       ((eq 'code (overlay-get ov 'hs))
        (let* ((nmb-line (count-lines (overlay-start ov)
                                      (overlay-end ov)))
               (display-string (format "(%d)..." nmb-line)))
          ;; fringe indicator
          (when +hs-folding-fringe-indicators
            (put-text-property 0 marker-length 'display
                               (list 'left-fringe
                                     '+hs-folding-fringe-marker
                                     '+hs-folding-fringe-face)
                               marker-string)
            (overlay-put ov 'before-string marker-string)
            (overlay-put ov '+hs-fringe t))
          ;; folding indicator
          (put-text-property 0 (length display-string)
                             'face '+hs-folding-face
                             display-string)
          (put-text-property 0 (length display-string)
                             'mouse-face 'highlight display-string)
          (overlay-put ov 'display display-string)
          (overlay-put ov '+hs-folded t)))
       ;; for docstring and comments, we don't display the number of
       line
       ((or (eq 'docstring (overlay-get ov 'hs))
            (eq 'comment (overlay-get ov 'hs)))
        (let ((display-string "..."))
          (put-text-property 0 (length display-string)
                             'mouse-face 'highlight display-string)
          (overlay-put ov 'display display-string)
          (overlay-put ov '+hs-folded t))))))

  (setq hs-set-up-overlay #'+hs-display-code-line-counts))

(eat-package xref
  :init
  (global-unset-key (kbd "C-<down-mouse-1>"))
  (global-set-key (kbd "C-<mouse-1>") #'xref-find-definitions-at-mouse)

  (setq
   xref-prompt-for-identifier nil
   xref-search-program 'ripgrep
   xref-show-xrefs-function #'xref-show-definitions-completing-read
   xref-show-definitions-function #'xref-show-definitions-completing-read))

(eat-package winner
  :hook (after-init-hook . winner-mode)
  :init
  (setq winner-dont-bind-my-keys t))

(eat-package smerge-mode
  :hook (find-file-hook . (lambda ()
                            (save-excursion
                              (goto-char (point-min))
                              (when (re-search-forward "^<<<<<<< " nil t)
                                (smerge-mode 1)))))
  :config
  (define-key smerge-mode-map (kbd "M-r") #'smerge-refine)
  (define-key smerge-mode-map (kbd "M-RET") #'smerge-keep-current))

(eat-package dired
  :init
  (setq
   dired-dwim-target t
   dired-kill-when-opening-new-dired-buffer t
   dired-listing-switches "-AGhlv"
   delete-by-moving-to-trash t)
  :config
  (setq
   dired-recursive-deletes 'top)
  ;; Prefer g-prefixed coreutils version of standard utilities when available
  (let ((gls (executable-find "gls")))
    (when gls (setq insert-directory-program gls)))
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "C-c C-p") #'wdired-change-to-wdired-mode))

(eat-package ibuffer
  :init
  (fset 'list-buffers 'ibuffer))

(eat-package ediff
  :init
  (setq
   ediff-window-setup-function #'ediff-setup-windows-plain
   ediff-highlight-all-diffs t
   ediff-split-window-function 'split-window-horizontally
   ediff-merge-split-window-function 'split-window-horizontally))

(eat-package flyspell
  :init
  ;; `flyspell' -- only enable in magit commit
  (setq flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil
        ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
  :config
  (setq flyspell-mode-map nil))

(eat-package project
  :init
  ;; `project'
  (defun my/project-files-in-directory (dir)
    "Use `fd' to list files in DIR."
    (let* ((default-directory dir)
           (localdir (file-local-name (expand-file-name dir)))
           (command (format "fd -H -t f -0 . %s" localdir)))
      (project--remote-file-names
       (sort (split-string (shell-command-to-string command) "\0" t)
             #'string<))))

  (when (executable-find "fd")
    (cl-defmethod project-files ((project (head local)) &optional dirs)
      "Override `project-files' to use `fd' in local projects."
      (mapcan #'my/project-files-in-director
              (or dirs (list (project-root project))))))

  (defun +project-name ()
    (file-name-nondirectory (directory-file-name (project-root (project-current))))))

(eat-package tab-bar
  :init
  (setq
   tab-bar-border nil
   tab-bar-close-button nil
   tab-bar-back-button nil
   tab-bar-new-button nil
   tab-bar-format '(tab-bar-format-tabs)
   tab-bar-tab-name-format-function '+tab-bar-tab-format-function
   tab-bar-separator ""
   tab-bar-tab-name-truncated-max 10)

  (custom-set-faces
   `(tab-bar ((t (:height 1.2 :family ,(format "%s-10" +font-variable-pitch))))))

  (defun +tab-bar-switch-project ()
    "Switch to project in a new tab, project name will be used as tab name.

No tab will created if the command is cancelled."
    (interactive)
    (let (succ)
      (unwind-protect
          (progn
            (tab-bar-new-tab)
            (call-interactively #'project-switch-project)
            (when-let ((proj (project-current)))
              (tab-bar-rename-tab (format "%s" (file-name-nondirectory (directory-file-name (cdr proj)))))
              (setq succ t)))
        (unless succ
          (tab-bar-close-tab)))))

  (defun +tab-bar-tab-format-function (tab i)
    (let ((current-p (eq (car tab) 'current-tab)))
      (propertize (concat
                   "    "
                   (alist-get 'name tab)
                   "    ")
                  'face
                  (funcall tab-bar-tab-face-function tab))))
  :config
  (global-set-key (kbd "C-x t .") #'tab-bar-rename-tab)
  (global-set-key (kbd "C-x t l") #'+tab-bar-switch-project))

(eat-package shell
  :init
  ;; Used as a `sh-mode' REPL.
  ;;
  ;; `shell' is recommended to use over `tramp'.
  (defun shell-toggle ()
    "Toggle a persistent shell popup window.
If popup is visible but unselected, select it.
If popup is focused, kill it."
    (interactive)
    (if-let ((win (get-buffer-window "*shell-popup*")))
        (when (eq (selected-window) win)
          ;; If users attempt to delete the sole ordinary window, silence it.
          (ignore-errors (delete-window win)))
      (let ((display-comint-buffer-action '(display-buffer-at-bottom
                                            (inhibit-same-window . nil))))

        (shell "*shell-popup*"))))
  (global-set-key (kbd "M-`") #'shell-toggle))

(eat-package cc-mode
  :init
  (setq c-default-style "linux")
  (setq-default c-basic-offset 4))

(eat-package python
  :init
  (setq
   python-indent-offset 4
   python-shell-completion-native-enable nil
   python-shell-interpreter "ipython"
   python-indent-guess-indent-offset nil))

(eat-package sql
  :init
  (setq
   sql-mysql-login-params '(user password server database port)))

(provide 'init-builtin)
