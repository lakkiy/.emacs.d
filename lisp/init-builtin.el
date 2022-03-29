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

(eat-package saveplace ;; TODO
  :hook (after-init-hook . save-place-mode))

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
  (defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))

  (defface hideshow-border-face
    '((((background light))
       :background "light coral" :extend t)
      (t
       :background "firebrick" :extend t))
    "Face used for hideshow fringe."
    :group 'hideshow)

  (define-fringe-bitmap 'hideshow-folded-fringe
    (vector #b00000000
            #b00000000
            #b00000000
            #b11000011
            #b11100111
            #b01111110
            #b00111100
            #b00011000))

  (defun hideshow-folded-overlay-fn (ov)
    "Display a folded region indicator with the number of folded lines."
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
             (info (format "(%d)..." nlines)))
        ;; fringe indicator
        (overlay-put ov 'before-string (propertize " "
                                                   'display '(left-fringe hideshow-folded-fringe
                                                                          hideshow-border-face)))
        ;; folding indicator
        (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))
  (setq hs-set-up-overlay #'hideshow-folded-overlay-fn))

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

(eat-package dired
  :hook (dired-mode-hook . dired-hide-details-mode)
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
  (fset 'list-buffers 'ibuffer)
  (setq-default ibuffer-show-empty-filter-groups nil)

  ;; Modify the default ibuffer-formats (toggle with `)
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 22 22 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 12 12 :left :elide)
                " "
                vc-relative-file)
          (mark modified read-only vc-status-mini " "
                (name 22 22 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 14 14 :left :elide)
                " "
                (vc-status 12 12 :left)
                " "
                vc-relative-file)))

  (setq ibuffer-filter-group-name-face 'font-lock-doc-face)
  :config
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size))))

(eat-package ediff
  :init
  (defvar local-ediff-saved-window-conf nil)

  (defun ediff-save-window-conf ()
    (setq local-ediff-saved-window-conf (current-window-configuration)))

  (defun ediff-restore-window-conf ()
    (when (window-configuration-p local-ediff-saved-window-conf)
      (set-window-configuration local-ediff-saved-window-conf)))

  (setq
   ediff-window-setup-function #'ediff-setup-windows-plain
   ediff-highlight-all-diffs t
   ediff-split-window-function 'split-window-horizontally
   ediff-merge-split-window-function 'split-window-horizontally)
  :config
  ;; Restore window config after quitting ediff
  (add-hook 'ediff-before-setup-hook #'ediff-save-window-conf)
  (add-hook 'ediff-quit-hook #'ediff-restore-window-conf))

(eat-package flyspell
  :init
  ;; `flyspell' -- only enable in magit commit
  (setq flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil
        ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
  :config
  (setq flyspell-mode-map nil))

;;; project managent
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
            (when-let ((proj (project-root (project-current))))
              (tab-bar-rename-tab (format "%s" (file-name-nondirectory (directory-file-name proj))))
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


;;; shell
;; The interactive shell.
;;
;; It can be used as a `sh-mode' REPL.
;;
;; `shell' is recommended to use over `tramp'.
(eat-package shell
  :hook
  (comint-mode-hook
   . (my-comint-init revert-tab-width-to-default))
  :init
  (defun my-comint-init ()
    (setq-local
     comint-input-ignoredups t
     comint-process-echoes t))

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
  (global-set-key (kbd "M-`") #'shell-toggle)

  ;; Correct indentation for `ls'
  (defun revert-tab-width-to-default ()
    "Revert `tab-width' to default value."
    (setq-local tab-width 8)))

(eat-package eshell)

(eat-package term
  :init
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

;;; programming
(eat-package paren
  :hook (prog-mode-hook . show-paren-mode)
  :init
  (setq
   show-paren-when-point-in-periphery t
   show-paren-when-point-inside-paren t
   ;; NOTE emacs 29
   show-paren-context-when-offscreen t))

(eat-package elec-pair
  :hook (prog-mode-hook . electric-pair-mode)
  :init
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(eat-package smerge-mode
  :hook (find-file-hook . (lambda ()
                            (save-excursion
                              (goto-char (point-min))
                              (when (re-search-forward "^<<<<<<< " nil t)
                                (smerge-mode 1)))))
  :config
  (define-key smerge-mode-map (kbd "M-r") #'smerge-refine)
  (define-key smerge-mode-map (kbd "M-RET") #'smerge-keep-current))

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

;; TODO init-sql.el
(eat-package sql
  :init
  (setq
   sql-mysql-login-params '(user password server database port))
  ;; `sql-connect' don't create new frame
  (add-to-list 'same-window-buffer-names "*SQL*"))

;;; search
(eat-package isearch
  :init
  (setq
   isearch-lazy-count t
   isearch-lazy-highlight t
   lazy-count-prefix-format nil
   lazy-count-suffix-format " [%s/%s]"
   lazy-highlight-buffer t
   lazy-highlight-cleanup nil
   ;; Record isearch in minibuffer history, so C-x ESC ESC can repeat it.
   isearch-resume-in-command-history t
   ;; M-< and M-> move to the first/last occurrence of the current search string.
   isearch-allow-motion t
   isearch-motion-changes-direction t)
  :config
  (define-advice isearch-occur (:after (_regexp &optional _nlines))
    (isearch-exit))
  (define-key isearch-mode-map (kbd "C-c C-o") #'isearch-occur)
  (define-key isearch-mode-map [escape] #'isearch-cancel)
  ;; Edit the search string instead of jumping back
  (define-key isearch-mode-map [remap isearch-delete-chac] #'isearch-del-chac))


;;; browse
(eat-package xwidget
  :init
  (setq browse-url-browser-function 'xwidget-webkit-browse-url)
  ;; run `xwidget-webkit-browse-url' in other tab
  (advice-add 'xwidget-webkit-browse-url :before #'(lambda (url &optional new-session)
                                                     (tab-bar-select-tab-by-name "xwidget")))
  :config
  (define-key xwidget-webkit-mode-map (kbd "y") #'xwidget-webkit-copy-selection-as-kill))

(eat-package webjump
  :init
  (global-set-key (kbd "C-c C-/") #'webjump)
  (setq webjump-sites
        '(("Emacs Wiki" .
           [simple-query "www.emacswiki.org" "www.emacswiki.org/cgi-bin/wiki/" #1=""])
          ("Emacs China" . "emacs-china.org")
          ("Emacs Reddit" . "www.reddit.com/r/emacs/")
          ("Emacs News" . "sachachua.com/blog/category/emacs-news/")
          ("Github" .
           [simple-query "github.com" "github.com/search?q=" #1#])
          ("DuckDuckGo" .
           [simple-query "duckduckgo.com" "duckduckgo.com/?q=" #1#])
          ("Google" .
           [simple-query "google.com" "google.com/search?q=" #1#])
          ("Youtube" .
           [simple-query "youtube.com" "youtube.com/results?search_query=" #1#])
          ("Google Groups" .
           [simple-query "groups.google.com" "groups.google.com/groups?q=" #1#])
          ("Wikipedia" .
           [simple-query "wikipedia.org" "wikipedia.org/wiki/" #1#]))))


(eat-package display-fill-column-indicator
  :hook (prog-mode-hook . display-fill-column-indicator-mode)
  :init
  (setq-default indicate-buffer-boundaries 'left)
  (setq-default display-fill-column-indicator-character ?\u254e))

;;; `modus-theme'
(setq modus-themes-mode-line '(accented barderless))

;;; save session
(eat-package desktop
  :hook (desktop-after-read-hook . desktop-load-theme)
  :init
  (setq desktop-path (list user-emacs-directory)
        desktop-auto-save-timeout 600)
  (desktop-save-mode 1)

  ;; Reload theme after `desktop-read'.
  ;; But it doesn't prevent the desktop-save-mode from saving the theme
  ;; in the .desktop file, instead it restores the theme after loading
  ;; the desktop.
  (defun desktop-load-theme ()
    "load custom theme"
    (interactive)
    (dolist (th custom-enabled-themes)
      (load-theme th)))

  (defun sanityinc/time-subtract-millis (b a)
    (* 1000.0 (float-time (time-subtract b a))))

  (defun sanityinc/desktop-time-restore (orig &rest args)
    (let ((start-time (current-time)))
      (prog1
          (apply orig args)
        (message "Desktop restored in %.2fms"
                 (sanityinc/time-subtract-millis (current-time)
                                                 start-time)))))
  (advice-add 'desktop-read :around 'sanityinc/desktop-time-restore)

  (defun sanityinc/desktop-time-buffer-create (orig ver filename &rest args)
    (let ((start-time (current-time)))
      (prog1
          (apply orig ver filename args)
        (message "Desktop: %.2fms to restore %s"
                 (sanityinc/time-subtract-millis (current-time)
                                                 start-time)
                 (when filename
                   (abbreviate-file-name filename))))))
  (advice-add 'desktop-create-buffer :around 'sanityinc/desktop-time-buffer-create)

  ;; save a bunch of variables to the desktop file
  ;; for lists specify the len of the maximal saved data also
  (setq desktop-globals-to-save
        '((comint-input-ring        . 50)
          (compile-history          . 30)
          desktop-missing-file-warning
          custom-enabled-themes
          (dired-regexp-history     . 20)
          (extended-command-history . 30)
          (face-name-history        . 20)
          (file-name-history        . 100)
          (grep-find-history        . 30)
          (grep-history             . 30)
          (magit-revision-history   . 50)
          (minibuffer-history       . 50)
          (org-clock-history        . 50)
          (org-refile-history       . 50)
          (org-tags-history         . 50)
          (query-replace-history    . 60)
          (read-expression-history  . 60)
          (regexp-history           . 60)
          (regexp-search-ring       . 20)
          register-alist
          (search-ring              . 20)
          (shell-command-history    . 50)
          tags-file-name
          tags-table-list)))


(eat-package savehist
  :hook (after-init-hook . savehist-mode)
  :init
  ;; Restore histories and registers after saving
  (setq-default history-length 1000))

;;; outline
(eat-package outline
  :init
  (setq
   outline-minor-mode-cycle t
   outline-minor-mode-highlight t))

;;;; Frame
(eat-package fullframe :straight t)
(with-eval-after-load 'ibuffer
  (fullframe ibuffer ibuffer-quit))

;;;; Info
(eat-package info
  :hook (Info-mode-hook . (lambda ()
                            (variable-pitch-mode)
                            (text-scale-increase 2))))

;;; init-builtin.el ends here
(provide 'init-builtin)
