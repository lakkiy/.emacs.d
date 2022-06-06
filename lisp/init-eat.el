;;; -*- lexical-binding: t -*-
;;; Basic
;;;; Variables

(defvar eat/user-full-name "Liu Bo")

(defvar eat/user-mail-address "liubolovelife@gmail.com")

(defvar eat/complete-delay 0.5
  "Delay time before complete.")

;;;; Consts
(defconst eat/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst eat/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst eat/emacs29p
  (>= emacs-major-version 29)
  "Emacs is 29 or above.")


;;; Frame
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(defvar eat/after-make-console-frame-hooks '()
  "Hooks to run after creating a new TTY frame")
(defvar eat/after-make-window-system-frame-hooks '()
  "Hooks to run after creating a new window-system frame")

(defun eat/run-after-make-frame-hooks (frame)
  "Run configured hooks in response to the newly-created FRAME.
Selectively runs either `eat/after-make-console-frame-hooks' or
`eat/after-make-window-system-frame-hooks'"
  (with-selected-frame frame
    (run-hooks (if window-system
                   'eat/after-make-window-system-frame-hooks
                 'eat/after-make-console-frame-hooks))))

(add-hook 'after-make-frame-functions 'eat/run-after-make-frame-hooks)

(defconst eat/initial-frame (selected-frame)
  "The frame (if any) active during Emacs initialization.")

(add-hook 'after-init-hook
          (lambda () (when eat/initial-frame
                       (eat/run-after-make-frame-hooks eat/initial-frame))))


;;; Font
(defvar eat/fonts-default
  '("Roboto Mono" "Iosevka" "Menlo" "Source Code Pro")
  "First installed font will be set to default font.")

(defvar eat/fonts-unicode
  '("Apple Color Emoji" "Noto Color Emoji")
  "First installed font will be set to unicode font.")

(defvar eat/fonts-cn
  '("LXGW WenKai" "PingFang SC")
  "First installed font will be set to Chinese font.")

(defvar eat/fonts-variable-pitch
  '("Cardo" "Bookerly" "Nimbus Sans" "Helvetica")
  "First installed font will be set to variable font.")

(defvar eat/font-size 12
  "Default font size.")

(defun eat/font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun eat/font-installed (list)
  "Return first installed font from LIST."
  (catch 'value
    (dolist (font list)
      (when (eat/font-installed-p font)
        (throw 'value font)))))

(defconst eat/font-default
  (eat/font-installed eat/fonts-default)
  "Default font.")

(defconst eat/font-unicode
  (eat/font-installed eat/fonts-unicode)
  "Unicode font.")

(defconst eat/font-cn
  (eat/font-installed eat/fonts-cn)
  "Chinese font.")

(defconst eat/font-variable-pitch
  (eat/font-installed eat/fonts-variable-pitch)
  "Variable pitch font.")

(defun eat/load-base-font ()
  (let ((font-spec (format "%s-%d" eat/font-default eat/font-size)))
    (set-frame-font font-spec)
    (set-face-attribute 'default nil :font font-spec)
    (add-to-list 'default-frame-alist `(font . ,font-spec)))
  (set-fontset-font t '(#x4e00 . #x9fff) eat/font-cn))

(defun eat/load-face-font ()
  (set-face-attribute 'variable-pitch nil :font eat/font-variable-pitch :height 1.3)
  (set-face-attribute 'fixed-pitch nil :font eat/font-default)
  (set-face-attribute 'fixed-pitch-serif nil :font eat/font-default)
  ;; make mode line use variable font but use original height
  (custom-set-faces
   `(mode-line ((t (:family ,eat/font-variable-pitch))))
   `(mode-line-inactive ((t (:family ,eat/font-variable-pitch))))))

(defun eat/load-ext-font ()
  (let ((font (frame-parameter nil 'font))
        (font-spec (font-spec :family eat/font-unicode)))
    (dolist (charset '(kana han hangul cjk-misc bopomofo symbol))
      (set-fontset-font font charset font-spec)))
  (set-fontset-font t 'emoji (font-spec :family eat/font-unicode) nil 'prepend)
  (setf (alist-get eat/font-unicode face-font-rescale-alist 0.7 nil 'string=) 0.7))

(defun eat/load-font ()
  (eat/load-base-font)
  (eat/load-face-font)
  (eat/load-ext-font))

(add-hook 'eat/after-make-window-system-frame-hooks (lambda ()
                                                      (eat/load-font)))


;;; Theme
;;;; Variables

(defvar eat/theme 'modus-operandi
  "Default theme.")

(defvar eat/theme-tui 'modus-vivendi
  "Default theme in terminal.")

(defvar eat/theme-system-light 'modus-operandi
  "Default light theme after system appearance changed.")

(defvar eat/theme-system-dark 'modus-vivendi
  "Default dark theme after system appearance changed.")

(defvar eat/theme-hooks nil
  "((theme-id . function) ...).")

;;;; Functions

(defun eat/load-theme (theme)
  "Load THEME without confirm."
  (load-theme theme t))

(defun eat/load-theme-advice (f theme-id &optional no-confirm no-enable &rest args)
  "Enhance `load-theme' by disabling other enabled themes & calling hooks."
  (unless no-enable ;
    (mapc #'disable-theme custom-enabled-themes))
  (prog1
      (apply f theme-id t no-enable args)
    (unless no-enable ;
      (pcase (assq theme-id eat/theme-hooks)
        (`(,_ . ,f) (funcall f))))))
(advice-add 'load-theme :around #'eat/load-theme-advice)

(defun eat/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha-background) 100))
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha-background newalpha))))))
(global-set-key (kbd "M-C-8") (lambda () (interactive) (eat/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (eat/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha-background . 100)))))

(add-hook 'eat/after-make-console-frame-hooks (lambda ()
                                                (when (fboundp 'menu-bar-mode)
                                                  (menu-bar-mode -1))
                                                (eat/load-theme eat/theme-tui)))

(add-hook 'eat/after-make-window-system-frame-hooks (lambda ()
                                                      (eat/load-theme eat/theme)))


;;; Optimization
(setq package-enable-at-startup nil
      frame-inhibit-implied-resize t
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      default-frame-alist '((scroll-bar-mode . 0)
                            (vertical-scroll-bars . nil)
                            (menu-bar-lines . 0)
                            (tool-bar-lines . 0)))

(defun eat/show-startup-time ()
  "Print startup time."
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))
(add-hook 'emacs-startup-hook #'eat/show-startup-time)

;; GC automatically while unfocusing the frame
(add-function :after after-focus-change-function
              (lambda ()
                (unless (frame-focus-state)
                  (garbage-collect))))

;; Speed up startup
(setq auto-mode-case-fold nil)

;; Optimization
(setq idle-update-delay 1.0)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; The nano style for truncated long lines.
(setq auto-hscroll-mode 'current-line)

(when eat/emacs29p
  ;; for mouse scroll
  (setq pixel-scroll-precision-large-scroll-height 60
        pixel-scroll-precision-interpolation-factor 30.0
        dired-mouse-drag-files t
        mouse-drag-and-drop-region t
        mouse-drag-and-drop-region-cross-program t)
  (add-hook 'after-init-hook (lambda () (pixel-scroll-precision-mode))))

;; scroll nand hscroll
(setq-default
 scroll-step 2
 scroll-margin 2
 hscroll-step 2                                     ; Horizontal Scroll
 hscroll-margin 2
 scroll-conservatively 101
 scroll-up-aggressively 0.01
 scroll-down-aggressively 0.01
 scroll-preserve-screen-position 'always
 auto-window-vscroll nil
 fast-but-imprecise-scrolling nil
 mouse-wheel-scroll-amount '(1 ((shift) . hscroll)) ; use shift + mouse wheel to scrll horizontally
 mouse-wheel-progressive-speed nil)

;; Contrary to what many Emacs users have in their configs, you don't need
;; more than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")

;; Disable cursor blink
(add-hook 'on-init-ui-hook (lambda () (blink-cursor-mode -1)))

;; Do not show cursor in nonselected windows
(setq-default cursor-in-non-selected-windows nil)

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
(defun display-startup-echo-area-message()
  (message nil))

;; indent with whitespace by default
(setq-default
 tab-width 4
 indent-tabs-mode nil)

;; Disable default auto backup and save file
(setq-default
 create-lockfiles nil                               ; Don't create lockfiles
 make-backup-files nil                              ; Disable auto save and backup
 auto-save-default nil
 auto-save-list-file-prefix nil)

(setq
 initial-scratch-message (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n")
 initial-major-mode 'fundamental-mode               ; Don't use prog-mode an stratup
 ring-bell-function 'ignore
 read-process-output-max (* 4 1024 1024)
 suggest-key-bindings nil                           ; Disable "You can run the command balabala..."
 word-wrap-by-category t                            ; Emacs 之光！
 use-short-answers t                                ; yse-or-no -> y-or-n
 suggest-key-bindings nil
 )

(setq-default
 inhibit-compacting-font-caches t                   ; Don’t compact font caches during GC.
 require-final-newline t                            ; add final newline
 visible-cursor t
 bidi-inhibit-bpa t                                 ; Improve long line display performance
 bidi-paragraph-direction 'left-to-right
 echo-keystrokes 0.01                               ; don't wait for keystrokes display
 warning-suppress-log-types '((comp))               ; Don't display compile warnings
 truncate-partial-width-windows 65                  ; Don't truncate lines in a window narrower than 65 chars.
 vc-follow-symlinks t                               ; always follow link
 server-client-instructions nil                     ; no client startup messages
 split-height-threshold nil                         ; prefer horizental split
 split-width-threshold 120
 )


;;; eat-package
;;;; Commentary:

;; Base on luna-load-package.el

;;;; Code:

(require 'pcase)

;;;; Variables

(defvar eat-all-packages-daemon t
  "If it's value is t, all package in `eat-package' will be required in dameon.")

(defconst eat--all-packages-p
  (and eat-all-packages-daemon (daemonp))
  "")

;;;; Functions

(defun eat-package-split-command-args (args)
  "Split args into commands and args.
If ARGS is (:command args args args :command args),
return: ((:command . (args args args)) (:command . (args)))."
  (let (ret-list arg-list command)
    (dolist (token (append args '(:finish)))
      (if (keywordp token)
          ;; Finish previous command
          (progn (if command (push (cons command (reverse arg-list))
                                   ret-list))
                 (setq arg-list nil)
                 ;; Start new command
                 (setq command token))
        (push token arg-list)))
    (reverse ret-list)))

(defun eat-package--handle-hook (arg-list package)
  "Handle hook arguments.
Each ARG in ARG-LIST is a cons (HOOK . FUNCTION).
HOOK can be either a single hook or a list of hooks.
FUNCTION can also be either a single function or a list of them.
PACKAGE is the package we are configuring."
  (let (ret-list hook-list func-list)
    (dolist (arg arg-list)
      (let ((hook (car arg))
            (func (cdr arg)))
        ;; Normalize to lists.
        (setq hook-list
              (if (symbolp hook) (list hook) hook))
        (setq func-list
              (if (or (symbolp func)
                      ;; Handle lambda correctly.
                      (functionp func))
                  (list func) func)))
      ;; Produce add-hook forms.
      (dolist (func func-list)
        ;; If FUNC is a lambda function, we can't autoload it,
        ;; Make it load the package before execution.
        (let ((func (if (not (symbolp func))
                        ;; We don't want closure.
                        `(lambda () (require ',package) (funcall ,func))
                      func)))
          (dolist (hook hook-list)
            (push `(add-hook ',hook #',func) ret-list)))))
    (reverse ret-list)))

(defun eat-package--collect-autoload (arg-list package)
  "Collect functions that needs autoload from ARG-LIST.
PACKAGE is the package we are loading.
Return a list of (autoload ...) forms."
  (let ((autoload
          (mapcan (lambda (arg)
                    (let ((command (car arg))
                          (arg-list (cdr arg)))
                      (pcase command
                        ;; ARG is either (hook . fn) or
                        ;;               ((hook ...) . fn) or
                        ;;               (hook . (fn ...))
                        (:hook (mapcan (lambda (arg)
                                         (let ((fn (cdr arg)))
                                           (if (or (symbolp fn)
                                                   ;; Handle lambda.
                                                   (functionp fn))
                                               (list fn)
                                             fn)))
                                       arg-list))
                        ;; ARG is either ".xxx" or (".xxx" . mode)
                        (:mode (mapcar (lambda (arg)
                                         (if (stringp arg)
                                             package
                                           (cdr arg)))
                                       arg-list)))))
                  arg-list)))
    (mapcar (lambda (fn)
              (if (symbolp fn)
                  `(autoload #',fn ,(symbol-name package) nil t)))
            autoload)))

(defmacro eat-package (package &rest args)
  "Like ‘use-package’.
PACKAGE is the package you are loading.
Available COMMAND:

  :init         Run right away.
  :config       Run after package loads.
  :hook         Each arguments is (HOOK . FUNC)
                HOOK and FUNC can be a symbol or a list of symbols.
  :mode         Add (ARG . PACKAGE) to ‘auto-mode-alist’. If ARG is
                already a cons, add ARG to ‘auto-mode-alist’.
  :commands     Add autoload for this command.
  :after        Require after this package loads.
  :reqire       Require this package right now.
  :straight     Install package via straight

Each COMMAND can take zero or more ARG. Among these commands,
:hook, :commands, and :after expect literal arguments, :init,
:config expect s-expressions, which are evaluated after
expansion of the macro.

ARGS.

\(fn PACKAGE &rest [COMMAND [ARG ...]] ...)"
  (declare (indent 1))
  ;; Group commands and arguments together.
  (let* ((arg-list (eat-package-split-command-args args))
         ;; Translate commands & arguments to valid
         ;; config code.
         (body
          (mapcan
           (lambda (arg)
             (let ((command (car arg))
                   (arg-list (cdr arg)))
               (pcase command
                 (:straight `((if (listp ',@arg-list)
                                  (straight-use-package ',@arg-list)
                                (straight-use-package ',package))))
                 (:init arg-list)
                 (:config `((with-eval-after-load ',package
                              ,@arg-list)))
                 (:hook (eat-package--handle-hook arg-list package))
                 (:mode
                  ;; ARG is either ".xxx" or (".xxx" . mode)
                  (mapcar
                   (lambda (arg)
                     (let ((pattern (if (consp arg) (car arg) arg))
                           (mode-fn (if (consp arg) (cdr arg) package)))
                       `(add-to-list 'auto-mode-alist
                                     ',(cons pattern mode-fn))))
                   arg-list))
                 (:commands
                  (mapcar (lambda (cmd)
                            `(autoload ',cmd ,(symbol-name package) nil t))
                          arg-list))
                 (:after
                  (mapcar (lambda (pkg)
                            `(with-eval-after-load ',pkg
                               (require ',package)))
                          arg-list)))))
           arg-list))
         (autoload-list (eat-package--collect-autoload arg-list package))
         ;; Must :require explicitly if you want to require this package.
         (require-p (let ((commands (mapcar #'car arg-list)))
                      (or (memq :require commands)))))
    `(condition-case err
         (progn
           ,@autoload-list
           ,@body
           (if eat--all-packages-p
               (require ',package)
             ,(when require-p `(require ',package))))
       ((debug error) (warn "Error when loading %s: %s" ',package
                            (error-message-string err))))))


;;; Mac specific configuration
(when eat/macp
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        ;; Render thinner fonts
        ns-use-thin-smoothing t
        ;; Don't open a file in a new frame
        ns-pop-up-frames nil)
  (push '(ns-transparent-titlebar . t) default-frame-alist)

  ;; https://emacs-china.org/t/emacs-mac-port-profile/2895/29?u=rua
  ;; NOTE: When PATH is changed, run the following command
  ;; $ sh -c 'printf "%s" "$PATH"' > ~/.path
  (condition-case err
      (let ((path (with-temp-buffer
                    (insert-file-contents-literally "~/.path")
                    (buffer-string))))
        (setenv "PATH" path)
        (setq exec-path (append (parse-colon-path path) (list exec-directory))))
    (error (warn "%s" (error-message-string err))))

  ;; load theme after system appearance changed
  (when (boundp 'ns-system-appearance)
    (add-to-list 'ns-system-appearance-change-functions
                 (lambda (l?d)
                   (if (eq l?d 'light)
                       (eat/load-theme eat/theme-system-light)
                     (eat/load-theme eat/theme-system-dark)))))

  (global-set-key [(super a)] #'mark-whole-buffer)
  (global-set-key [(super v)] #'yank)
  (global-set-key [(super c)] #'kill-ring-save)
  (global-set-key [(super s)] #'save-buffer)
  (global-set-key [(super l)] #'goto-line)
  (global-set-key [(super w)] #'delete-frame)
  (global-set-key [(super q)] #'save-buffers-kill-terminal) ;; `save-buffers-kill-emacs' will shutdown emacs daemon
  (global-set-key [(super z)] #'undo))


;;; Linux specific configuration
(when eat/linuxp
  ;; NOTE use C-M-8 to set manually
  ;; (push '(alpha-background . 80) default-frame-alist)
  ;; Linux specific
  (setq x-underline-at-descent-line t)

  (setq-default
   pgtk-use-im-context-on-new-connection nil          ; Don't use Fcitx5 in Emacs in PGTK build
   x-gtk-resize-child-frames nil)

  ;; Don't use GTK+ tooltip
  (when (boundp 'x-gtk-use-system-tooltips)
    (setq x-gtk-use-system-tooltips nil)))


;;; Dvorak
;; Make “C-t” act like “C-x”, so it's easier to type on Dvorak layout
(keyboard-translate ?\C-t ?\C-x)
(keyboard-translate ?\C-x ?\C-t)


;;; Keybindings
;; bind `describe-keymap', added in emacs 28
(global-set-key (kbd "C-h C-k") #'describe-keymap)

;; this will stuck emacs
(global-unset-key (kbd "C-h h"))


;;; Built-in packages
;; TODO enable `icomplete-vertical-mode' if just load one file
(eat-package icomplete)

(eat-package pulse
  :hook
  ((imenu-after-jump-hook isearch-update-post-hook)
   . eat/recenter-and-pulse)
  ((bookmark-after-jump  next-error)
   . eat/recenter-and-pulse-line)
  :init
  (custom-set-faces
   '(pulse-highlight-start-face ((t (:inherit region))))
   '(pulse-highlight-face ((t (:inherit region)))))

  (defun eat/pulse-momentary-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))

  (defun eat/pulse-momentary (&rest _)
    "Pulse the region or the current line."
    (if (fboundp 'xref-pulse-momentarily)
        (xref-pulse-momentarily)
      (eat/pulse-momentary-line)))

  (defun eat/recenter-and-pulse(&rest _)
    "Recenter and pulse the region or the current line."
    (recenter)
    (eat/pulse-momentary))

  (defun eat/recenter-and-pulse-line (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (eat/pulse-momentary-line))

  (dolist (cmd '(recenter-top-bottom
                 other-window windmove-do-window-select
                 pager-page-down pager-page-up))
    (advice-add cmd :after #'eat/pulse-momentary-line))

  (dolist (cmd '(pop-to-mark-command
                 pop-global-mark
                 goto-last-change))
    (advice-add cmd :after #'eat/recenter-and-pulse)))

(eat-package repeat
  :init
  (setq repeat-mode t
        repeat-keep-prefix t
        repeat-exit-timeout 3
        repeat-exit-key (kbd "RET")))

(eat-package mouse
  :hook (after-init-hook . context-menu-mode))

(eat-package minibuffer
  :init
  (setq
   completion-styles '(basic partial-completion)
   completion-category-overrides '((file (styles basic partial-completion)))
   completion-cycle-threshold t
   minibuffer-depth-indicate-mode t
   minibuffer-eldef-shorten-default t
   minibuffer-electric-default-mode t))

(eat-package display-line-numbers
  :init
  (setq display-line-numbers-width 3))

(eat-package hippie-exp
  :init
  (global-set-key [remap dabbrev-expand] #'hippie-expand)
  (global-set-key (kbd "M-/") 'hippie-expand)
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

(eat-package subword
  :hook (prog-mode-hook . subword-mode))

(eat-package simple
  :hook (before-save-hook . delete-trailing-whitespace)
  :init
  (setq visual-line-fringe-indicators '(nil right-curly-arrow)
        ;; List only applicable commands.
        read-extended-command-predicate #'command-completion-default-include-p
        fill-column 72))

(eat-package hl-line
  :hook
  ((prog-mode-hook conf-mode-hook) . hl-line-mode)
  :init
  (setq-default hl-line-sticky-flag nil))

(eat-package tramp
  :init
  (setq
   tramp-auto-save-directory temporary-file-directory
   ;; Always use file cache when using tramp
   remote-file-name-inhibit-cache nil
   ;; C-x C-f /ssh:
   tramp-default-method "ssh"
   vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                vc-ignore-dir-regexp
                                tramp-file-name-regexp))
  :config
  ;; use `magit' with yadm, (magit-status "/yadm::")
  (add-to-list 'tramp-methods
               '("yadm"
                 (tramp-login-program "yadm")
                 (tramp-login-args (("enter")))
                 (tramp-login-env (("SHELL") ("/bin/sh")))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-args ("-c"))))
  ;; ‘Private Directories’ are the settings of the $PATH environment,
  ;; as given in your ‘~/.profile’.  This entry is represented in
  ;; the list by the special value ‘tramp-own-remote-path’.
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(eat-package eldoc
  :init
  (setq eldoc-idle-delay 1))

(eat-package whitespace
  :hook
  ((prog-mode-hook conf-mode-hook) . whitespace-mode)
  :init
  (setq whitespace-style '(face trailing)))

(eat-package hideshow
  :hook (prog-mode-hook . hs-minor-mode)
  :init
  ;; FIXME
  (defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))

  (defface hideshow-border-face
    '((((background light))
       :background "rosy brown" :extend t)
      (t
       :background "sandy brown" :extend t))
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
             (info (format " (%d)..." nlines)))
        ;; fringe indicator
        (overlay-put ov 'before-string (propertize " "
                                                   'display '(left-fringe hideshow-folded-fringe
                                                                          hideshow-border-face)))
        ;; folding indicator
        (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))

  (setq hs-set-up-overlay #'hideshow-folded-overlay-fn))

(eat-package xref
  :hook
  ((xref-after-return-hook xref-after-jump-hook) . recenter)
  :init
  (global-unset-key (kbd "C-<down-mouse-1>"))
  (global-set-key (kbd "C-<mouse-1>") #'xref-find-definitions-at-mouse)
  (setq xref-prompt-for-identifier nil
        xref-search-program 'ripgrep
        xref-show-xrefs-function #'xref-show-definitions-completing-read
        xref-show-definitions-function #'xref-show-definitions-completing-read))

(eat-package dired
  :hook (dired-mode-hook . dired-hide-details-mode)
  :init
  (setq
   dired-dwim-target t
   dired-kill-when-opening-new-dired-buffer t
   dired-listing-switches "-AGhlv"
   delete-by-moving-to-trash t)
  :config
  (setq dired-recursive-deletes 'top)
  ;; Prefer g-prefixed coreutils version of standard utilities when available
  (let ((gls (executable-find "gls")))
    (when gls (setq insert-directory-program gls)))
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "C-c C-p") #'wdired-change-to-wdired-mode))

(eat-package ibuffer
  :init
  (fset 'list-buffers 'ibuffer)
  (setq-default ibuffer-show-empty-filter-groups nil)
  (global-set-key (kbd "C-x B") 'ibuffer)
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
  (fullframe ibuffer ibuffer-quit)
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size))))

(eat-package ediff
  :init
  (defvar local-ediff-saved-window-conf nil)

  (defun eat/ediff-save-window-conf ()
    (setq local-ediff-saved-window-conf (current-window-configuration)))

  (defun eat/ediff-restore-window-conf ()
    (when (window-configuration-p local-ediff-saved-window-conf)
      (set-window-configuration local-ediff-saved-window-conf)))

  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-highlight-all-diffs t
        ediff-split-window-function 'split-window-horizontally
        ediff-merge-split-window-function 'split-window-horizontally)
  :config
  ;; Restore window config after quitting ediff
  (add-hook 'ediff-before-setup-hook #'eat/ediff-save-window-conf)
  (add-hook 'ediff-quit-hook #'eat/ediff-restore-window-conf))

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
  (defun eat/project-name ()
    (file-name-nondirectory
     (directory-file-name
      (project-root
       (project-current)))))

  (defun eat/project-info ()
    (interactive)
    (message "%s" (project-current t)))

  (defun eat/add-dot-project ()
    (interactive)
    (let* ((root-dir (read-directory-name "Root: "))
           (f (expand-file-name ".project" root-dir)))
      (message "Create %s..." f)
      (make-empty-file f)))

  ;; do not remember tramp project
  (defun eat/project-remember-advice (fn pr &optional no-write)
    (let* ((remote? (file-remote-p (project-root pr)))
           (no-write (if remote? t no-write)))
      (funcall fn pr no-write)))
  (advice-add 'project-remember-project :around
              'eat/project-remember-advice)

  :config
  (defun eat/project-files-in-directory (dir)
    "Use `fd' to list files in DIR."
    (let* ((default-directory dir)
           (localdir (file-local-name (expand-file-name dir)))
           (command (format "fd -c never -H -t f -0 . %s" localdir)))
      (project--remote-file-names
       (sort (split-string (shell-command-to-string command) "\0" t)
             #'string<))))

  ;; use fd in `project-find-file'
  (when (executable-find "fd")
    (cl-defmethod project-files ((project (head local)) &optional dirs)
      "Override `project-files' to use `fd' in local projects."
      (mapcan #'eat/project-files-in-directory
              (or dirs (list (project-root project))))))

  (defun eat/project-try-local (dir)
    "Determine if DIR is a non-Git project."
    (catch 'ret
      (let ((pr-flags '((".project")
                        ("go.mod" "Cargo.toml" "project.clj" "pom.xml" "package.json") ;; higher priority
                        ("Makefile" "README.org" "README.md"))))
        (dolist (current-level pr-flags)
          (dolist (f current-level)
            (when-let ((root (locate-dominating-file dir f)))
              (throw 'ret (cons 'local root))))))))
  (cl-defmethod project-root ((project (head local)))
    (cdr project))
  (add-to-list 'project-find-functions #'eat/project-try-local t))

(eat-package tab-bar
  :init
  (setq tab-bar-border nil
        tab-bar-close-button nil
        tab-bar-back-button nil
        tab-bar-new-button nil
        tab-bar-format '(tab-bar-format-tabs)
        tab-bar-tab-name-format-function 'eat/tab-bar-tab-format-function
        tab-bar-separator ""
        tab-bar-tab-name-truncated-max 10)

  (custom-set-faces
   `(tab-bar ((t (:family ,eat/font-variable-pitch)))))

  (defun eat/tab-bar-switch-project ()
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

  (defun eat/tab-bar-tab-format-function (tab i)
    (let ((current-p (eq (car tab) 'current-tab)))
      (propertize (concat
                   "   "
                   (alist-get 'name tab)
                   "   ")
                  'face
                  (funcall tab-bar-tab-face-function tab))))
  :config
  (global-set-key (kbd "C-x t .") #'tab-bar-rename-tab)
  (global-set-key (kbd "C-x t l") #'eat/tab-bar-switch-project))

(eat-package paren
  :init
  (setq show-paren-when-point-in-periphery t
        show-paren-when-point-inside-paren t)
  (when eat/emacs29p
    (setq show-paren-context-when-offscreen t)))

(eat-package elec-pair
  :hook (prog-mode-hook . electric-pair-mode)
  :init
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(eat-package smerge-mode
  :hook (find-file-hook . (lambda ()
                            (save-excursion
                              (goto-char (point-min))
                              (when (re-search-forward "^<<<<<<< " nil t)
                                (smerge-mode 1))))))

(eat-package cc-mode
  :init
  (setq c-default-style "linux"
        c-basic-offset 4))

(eat-package python
  :init
  (setq python-indent-offset 4
        python-shell-completion-native-enable nil
        python-shell-interpreter "ipython"
        python-indent-guess-indent-offset nil))

(eat-package sql
  :init
  (setq sql-mysql-login-params '(user password server database port)))

(eat-package xwidget
  :init
  ;; use `eyebrowse' replace `tab-bar'
  ;; run `xwidget-webkit-browse-url' in other tab
  ;; (advice-add 'xwidget-webkit-browse-url :before #'(lambda (url &optional new-session)
  ;;                                                    "Run `xwidget-webkit-browse-url' in name tab 'xwidget'."
  ;;                                                    (tab-bar-select-tab-by-name "xwidget")))
  :config
  (define-key xwidget-webkit-mode-map (kbd "y") #'xwidget-webkit-copy-selection-as-kill))

(eat-package webjump
  :init
  (global-set-key (kbd "C-x C-/") #'webjump)
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
          ("stackoverflow" .
           [simple-query "stackoverflow.com" "stackoverflow.com/search?q=" #1#])
          ("Wikipedia" .
           [simple-query "wikipedia.org" "wikipedia.org/wiki/" #1#]))))

(progn
  (setq
   modus-themes-mode-line '(barderless)
   modus-themes-italic-constructs t
   modus-themes-bold-constructs t
   modus-themes-markup '(background italic)
   modus-themes-paren-match '(bold intense)
   modus-themes-links '(neutral-underline background)
   modus-themes-prompts '(intense bold)
   modus-themes-org-blocks 'gray-background
   modus-themes-region '(bg-only no-extend)
   modus-themes-headings
   '((1 . (1.15))
     (2 . (1.05))
     (t . (semibold))))
  (defun eat/custom-modus-operandi()
    (set-face-background 'cursor "deep pink")
    (set-face-foreground 'link "#0168da")
    (set-face-background 'isearch "#feff00")
    (set-face-foreground 'isearch nil)
    (set-face-background 'lazy-highlight "#feff00")
    (set-face-foreground 'font-lock-function-name-face "#0168da")
    (set-face-foreground 'font-lock-keyword-face "#874bf8")
    (set-face-foreground 'font-lock-comment-face "DarkGray")
    (set-face-foreground 'font-lock-constant-face "dark cyan")
    (set-face-foreground 'font-lock-string-face "chocolate")
    (with-eval-after-load 'org
      (set-face-foreground 'org-meta-line "Gray")
      (set-face-foreground 'org-drawer "Gray")
      (set-face-foreground 'org-document-info-keyword "Gray")
      (set-face-foreground 'org-date "Gray")
      (set-face-foreground 'org-link "#0168da")

      (set-face-attribute 'org-level-1 nil :foreground "#0168da")
      (set-face-attribute 'org-level-2 nil :foreground "#874bf8")
      (set-face-attribute 'org-level-3 nil :foreground "dark cyan")
      (set-face-attribute 'org-level-4 nil :foreground "violet red")
      (set-face-attribute 'org-level-5 nil :foreground "SpringGreen4")
      (set-face-attribute 'org-level-6 nil :foreground "orange red")
      (set-face-attribute 'org-level-7 nil :foreground "light sea green")
      (set-face-attribute 'org-level-8 nil :foreground "chocolate")

      (set-face-attribute 'org-headline-done nil :foreground "gray")))
  (add-hook 'eat/theme-hooks '(modus-operandi . eat/custom-modus-operandi)))

(eat-package outline
  :init
  (setq outline-minor-mode-cycle t
        outline-minor-mode-highlight t))

(eat-package info
  :hook (Info-mode-hook . variable-pitch-mode))

;; use C-q C-l to add page break symbol
(eat-package page)

(eat-package message
  :hook (message-mode-hook . auto-fill-mode)
  :init
  (setq user-full-name eat/user-full-name
        user-mail-address eat/user-mail-address
        message-kill-buffer-on-exit t
        message-mail-alias-type 'ecomplete
        message-send-mail-function #'message-use-send-mail-function
        message-signature user-full-name))

(eat-package sendmail
  :init
  (setq send-mail-function #'smtpmail-send-it))

(eat-package smtpmail
  :init
  (setq smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-user user-mail-address
        smtpmail-smtp-service 587
        smptmail-stream-type 'ssl))

(eat-package flymake
  :hook
  (prog-mode-hook . flymake-mode)
  (emacs-lisp-mode-hook . (lambda ()
                            (flymake-mode -1)))
  :init
  (setq-default flymake-diagnostic-functions nil)
  (defun sekiro-flymake-mode-line-format ()
    (let* ((counter (string-to-number
                     (nth 1
                          (cadr
                           (flymake--mode-line-counter :error t)))))
           (sekiro-flymake (when (> counter 0)
                             'compilation-error)))
      (propertize
       "危"
       'face
       sekiro-flymake)))
  (defun eat/flymake-mode ()
    (interactive)
    (add-hook 'prog-mode-hook #'flymake-mode)))


;;; init-eat.el ends here
(provide 'init-eat)
