;;; -*- lexical-binding: t -*-
;; config should work under emacs -Q like straight and gc...

;; DOOM core/core-packages.el#L87-L90
;; https://www.reddit.com/r/emacs/comments/mtb05k/emacs_init_time_decreased_65_after_i_realized_the/
(setq straight-check-for-modifications '(check-on-save find-when-checking))
(setq straight-vc-git-default-clone-depth 1)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

;; require all packages in emacsclient
(setq eat-all-packages-daemon t)
(require 'eat-package)

;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and (file-exists-p custom-file)
           (file-readable-p custom-file))
  (load custom-file :no-error :no-message))

(eat-package benchmark-init
  :straight
  (benchmark-init :type git :host github :repo "404cn/benchmark-init-el")
  :init
  (when +enable-benchmark
    (require 'benchmark-init))
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(eat-package gcmh
  :straight t
  :hook (after-init-hook . gcmh-mode)
  :init
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold #x6400000)) ;; 100 MB

(when sys/macp
  (eat-package exec-path-from-shell
    :straight t
    :init
    (add-hook 'after-init-hook #'exec-path-from-shell-initialize))

  (setq mac-option-modifier 'meta
        mac-command-modifier 'super)

  (global-set-key [(super a)] #'mark-whole-buffer)
  (global-set-key [(super v)] #'yank)
  (global-set-key [(super c)] #'kill-ring-save)
  (global-set-key [(super s)] #'save-buffer)
  (global-set-key [(super l)] #'goto-line)
  (global-set-key [(super w)] #'delete-frame)
  (global-set-key [(super q)] #'save-buffers-kill-emacs)
  (global-set-key [(super z)] #'undo)

  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))
  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
  ;; FIXME 乱码了
  ;; (setq interprogram-cut-function 'paste-to-osx)
  ;; (setq interprogram-paste-function 'copy-from-osx)

  ;; Render thinner fonts
  (setq ns-use-thin-smoothing t)
  ;; Don't open a file in a new frame
  (setq ns-pop-up-frames nil))

(unless sys/macp
  (setq command-line-ns-option-alist nil))

(when sys/linuxp
  ;; Linux specific
  (setq x-underline-at-descent-line t)

  (setq-default
   pgtk-use-im-context-on-new-connection nil          ; Don't use Fcitx5 in Emacs in PGTK build
   x-gtk-resize-child-frames nil)

  ;; Don't use GTK+ tooltip
  (when (boundp 'x-gtk-use-system-tooltips)
    (setq x-gtk-use-system-tooltips nil)))

(unless sys/linuxp
  (setq command-line-x-option-alist nil))

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)

            ;; GC automatically while unfocusing the frame
            ;; `focus-out-hook' is obsolete since 27.1
            (add-function :after after-focus-change-function
                          (lambda ()
                            (unless (frame-focus-state)
                              (garbage-collect))))))

;; From DoomEmacs
;; Contrary to what many Emacs users have in their configs, you don't need
;; more than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")

;; Speed up startup
(setq auto-mode-case-fold nil)

;; The nano style for truncated long lines.
;; See emacs lighting talk mengmeng.
(setq auto-hscroll-mode 'current-line)

;; in emacs29 from Po Lu!
(when (boundp pixel-scroll-precision-mode)
  ;; for mouse scroll
  (setq pixel-scroll-precision-large-scroll-height 60)
  (setq pixel-scroll-precision-interpolation-factor 30.0)
  (add-hook 'after-init-hook (lambda () (pixel-scroll-precision-mode))))

;; Disable cursor blink
(add-hook 'after-init-hook (lambda () (blink-cursor-mode -1)))

;; Optimization
(setq idle-update-delay 1.0)

;; Do not show cursor in nonselected windows
(setq-default cursor-in-non-selected-windows nil)

;; Do not highlight symbol in nonselected windows, see `highlight-symbol-at-point'
;; (setq highlight-nonselected-windows nil)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Suppress GUI features and more
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-splash-screen t
      inhibit-x-resources t
      inhibit-default-init t
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-buffer-menu t)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;; Pixelwise resize
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

;; Shut up!
(defun display-startup-echo-area-message() (message nil))

(setq-default
 initial-scratch-message (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n")
 initial-major-mode 'fundamental-mode
 inhibit-compacting-font-caches t                   ; Don’t compact font caches during GC.
 ring-bell-function 'ignore                         ; Disable osx bell ring
 require-final-newline t                            ; add final newline
 mouse-yank-at-point t                              ; Mouse yank at point instead of click position.
 comment-empty-lines t
 visible-cursor t
 bidi-inhibit-bpa t                                 ; Improve long line display performance
 bidi-paragraph-direction 'left-to-right
 echo-keystrokes 0.01                               ; don't wait for keystrokes display
 read-process-output-max (* 4 1024 1024)
 warning-suppress-log-types '((comp))               ; Don't display compile warnings
 truncate-partial-width-windows 65                  ; Don't truncate lines in a window narrower than 65 chars.
 vc-follow-symlinks t                               ; always follow link
 server-client-instructions nil                     ; no client startup messages
 use-short-answers t                                ; yse-or-no -> y-or-n
 split-height-threshold nil                         ; prefer horizental split
 split-width-threshold 120
 suggest-key-bindings nil                           ; disable "You can run the command balabala..."
 word-wrap-by-category t ;; Emacs 之光！
 )

;; indent with whitespace by default
(setq-default
 tab-width 4
 indent-tabs-mode nil)

;; disable default auto backup and save file
(setq-default
 create-lockfiles nil                               ; Don't create lockfiles
 make-backup-files nil                              ; Disable auto save and backup
 auto-save-default nil
 auto-save-list-file-prefix nil)

;; scroll nand hscroll
(setq-default
 scroll-step 1
 scroll-margin 10
 scroll-conservatively 100000
 scroll-up-aggressively 0.01
 scroll-down-aggressively 0.01
 scroll-preserve-screen-position t
 auto-window-vscroll nil
 fast-but-imprecise-scrolling nil
 mouse-wheel-scroll-amount '(1 ((shift) . hscroll)) ; use shift + mouse wheel to scrll horizontally
 mouse-wheel-progressive-speed nil
 hscroll-step 1                                     ; Horizontal Scroll
 hscroll-margin 10)

;; `minibuffer'
(setq
 ;; `selectrum', `vertico' and `icomplete' will honoring
 ;; completion-styles '(basic partial-completion substring flex)
 ;; completion-category-overrides '((buffer (styles . (flex))))
 completion-cycle-threshold t
 minibuffer-depth-indicate-mode t
 minibuffer-eldef-shorten-default t
 minibuffer-electric-default-mode t)

;; `recentf'
;; (add-hook 'after-init-hook #'recentf-mode)
(global-set-key (kbd "C-x C-r") #'recentf-open-files)

;; `display-line-numbers'
(setq-default
 display-line-numbers-width 3)

;; (add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; `subword'
(add-hook 'prog-mode-hook #'subword-mode)

;; `simple'
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq visual-line-fringe-indicators '(nil right-curly-arrow)
      ;; List only applicable commands.
      read-extended-command-predicate #'command-completion-default-include-p)

;; `so-long'
(add-hook 'after-init-hook #'global-so-long-mode)

;; `repeat'
(setq
 repeat-mode t
 repeat-keep-prefix t
 repeat-exit-timeout 3
 repeat-exit-key (kbd "RET"))

;; `hl-line'
(setq-default
 hl-line-sticky-flag nil)

(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'conf-mode-hook #'hl-line-mode)

;; `autorevert'
(add-hook 'after-init-hook #'global-auto-revert-mode)

;; `elec-pair'
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(add-hook 'prog-mode-hook #'electric-pair-mode)

;; `saveplace'
(add-hook 'after-init-hook #'save-place-mode)

;; `paren'
(setq
 show-paren-when-point-in-periphery t
 show-paren-when-point-inside-paren t
 ;; NOTE emacs 29
 show-paren-context-when-offscreen t)

(add-hook 'prog-mode-hook #'show-paren-mode)

;; `tramp'
(setq
 ;; Always use file cache when using tramp
 remote-file-name-inhibit-cache nil
 ;; C-x C-f /ssh:
 tramp-default-method "ssh")

;; `eldoc'
(setq eldoc-idle-delay 2)

;; `whitespace'
(setq whitespace-style '(face trailing))

(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'conf-mode-hook #'whitespace-mode)

;; `hideshow'
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

(setq hs-set-up-overlay #'+hs-display-code-line-counts)

(add-hook 'prog-mode-hook #'hs-minor-mode)

;; `xref'
(global-unset-key (kbd "C-<down-mouse-1>"))
(global-set-key (kbd "C-<mouse-1>") #'xref-find-definitions-at-mouse)

(setq
 xref-prompt-for-identifier nil
 xref-search-program 'ripgrep
 xref-show-xrefs-function #'xref-show-definitions-completing-read
 xref-show-definitions-function #'xref-show-definitions-completing-read)

;; `winner'
(setq winner-dont-bind-my-keys t)

(add-hook 'after-init-hook #'winner-mode)

;; `smerge-mode'
(add-hook 'find-file-hook
          #'(lambda ()
              (save-excursion
                (goto-char (point-min))
                (when (re-search-forward "^<<<<<<< " nil t)
                  (smerge-mode 1)))))

(with-eval-after-load 'smerge-mode
  (define-key smerge-mode-map (kbd "M-r") #'smerge-refine)
  (define-key smerge-mode-map (kbd "M-RET") #'smerge-keep-current))

;; `dired'
(setq
 dired-dwim-target t
 dired-kill-when-opening-new-dired-buffer t
 dired-recursive-deletes 'top
 dired-listing-switches "-AFhlv"
 delete-by-moving-to-trash t)

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c C-p") #'wdired-change-to-wdired-mode))

;; `ibuffer'
(fset 'list-buffers 'ibuffer)

;; `ediff'
(setq
 ediff-window-setup-function #'ediff-setup-windows-plain
 ediff-highlight-all-diffs t
 ediff-split-window-function 'split-window-horizontally
 ediff-merge-split-window-function 'split-window-horizontally)

;; `flyspell' -- only enable in magit commit
(setq flyspell-issue-welcome-flag nil
      flyspell-issue-message-flag nil
      ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))

(with-eval-after-load 'flyspell
  (setq flyspell-mode-map nil))

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
  (file-name-nondirectory (directory-file-name (project-root (project-current)))))

;; `tab-bar'
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
 `(tab-bar ((t (:height 1.4 :family ,(format "%s-10" +font-variable-pitch))))))

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
                 "┃   "
                 (alist-get 'name tab)
                 "    ")
                'face
                (funcall tab-bar-tab-face-function tab))))

(with-eval-after-load 'tab-bar
  (global-set-key (kbd "C-x t .") #'tab-bar-rename-tab)
  (global-set-key (kbd "C-x t l") #'+tab-bar-switch-project))

;; `modus-theme'
(setq modus-themes-mode-line '(accented barderless))

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
(global-set-key (kbd "M-`") #'shell-toggle)

;; `cc-mode'
(setq c-default-style "linux")
(setq-default c-basic-offset 4)

;; `python'
(setq
 python-indent-offset 4
 python-shell-completion-native-enable nil
 python-shell-interpreter "ipython"
 python-indent-guess-indent-offset nil)

(provide 'init-straight)
