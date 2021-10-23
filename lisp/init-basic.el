;;; -*- lexical-binding: t -*-

(require 'init-utils)

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst emacs/>=28p
  (>= emacs-major-version 28)
  "Emacs is 28 or above.")

(defvar +font "Monaco")
(defvar +font-cn "FZSuXinShiLiuKaiS-R-GB")
(defvar +font-unicode "Apple Color Emoji")
;; (defvar +font-variable-pitch "Bookerly" "Font use in variable-pitch-mode.")
(defvar +font-variable-pitch "Cardo" "Font use in variable-pitch-mode.")
(defvar +font-height (cond (sys/macp 130) (t 110)))
(defvar +use-header-line (if (display-graphic-p) nil t) "Wheather to use header line.")
(defvar +theme 'doom-spacegrey "Theme use in gui.")
(defvar +theme-tui 'kaolin-aurora "Theme use in tui.")
(defvar +theme-use-system nil)
(defvar +theme-system-light 'doom-solarized-light "Theme used after change system apperance to light.")
(defvar +theme-system-dark 'doom-solarized-dark "Theme used after change system apperance to dark.")
(defvar +enable-proxy? nil)
(defvar +proxy "127.0.0.1:7890")
(defvar +erc-password "")
(defvar +telega-proxy nil)
(defvar +use-icon-p nil)

(with-no-warnings
  ;; Don't ping things that look like domain names.
  (setq ffap-machine-p-known 'reject)

  (when (eq window-system 'mac)
    ;; Compatible with Emacs Mac port
    (setq mac-option-modifier 'meta
          mac-command-modifier 'super)
    (bind-keys ([(super a)] . mark-whole-buffer)
               ([(super c)] . kill-ring-save)
               ([(super l)] . goto-line)
               ([(super q)] . save-buffers-kill-emacs)
               ([(super s)] . save-buffer)
               ([(super v)] . yank)
               ([(super w)] . delete-frame)
               ([(super z)] . undo)))

  (unless sys/macp
    (setq command-line-ns-option-alist nil))
  (unless sys/linuxp
    (setq command-line-x-option-alist nil)))

(defvar +theme-hooks nil
  "((theme-id . function) ...)")
(defun +load-theme-advice (f theme-id &optional no-confirm no-enable &rest args)
  "Enhance `load-theme' by disabling other enabled themes & calling hooks"
  (unless no-enable ;
    (mapc #'disable-theme custom-enabled-themes))
  (prog1
      (apply f theme-id no-confirm no-enable args)
    (unless no-enable ;
      (pcase (assq theme-id +theme-hooks)
        (`(,_ . ,f) (funcall f))))))
(advice-add 'load-theme :around #'+load-theme-advice)

;; auto change theme after system apearance changed
(when (and (boundp 'ns-system-appearance)
           (display-graphic-p)
           +theme-use-system)
  (add-to-list 'ns-system-appearance-change-functions
               (lambda (l?d)
                 (if (eq l?d 'light)
                     (load-theme +theme-system-light t)
                   (load-theme +theme-system-dark t)))))

(when +enable-proxy?
  (add-hook 'after-init-hook (lambda () (+proxy-http-enable))))

(defun +reopen-file-with-sudo ()
  (interactive)
  (find-alternate-file (format "/sudo::%s" (buffer-file-name))))
(global-set-key (kbd "C-x C-z") #'+reopen-file-with-sudo)

(add-hook 'after-init-hook (lambda () (blink-cursor-mode -1)))

(fset 'list-buffers 'ibuffer)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Encoding
;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)

(setq-default
 initial-major-mode 'fundamental-mode
  inhibit-compacting-font-caches t                   ; Don’t compact font caches during GC.
 delete-by-moving-to-trash t                        ; Deleting files go to OS's trash folder
 ring-bell-function 'ignore                         ; Disable osx bell ring
 hl-line-sticky-flag nil
 create-lockfiles nil                               ; Don't create lockfiles
 require-final-newline t                            ; add final newline
 make-backup-files nil                              ; Disable auto save and backup
 auto-save-default nil
 auto-save-list-file-prefix nil
 mouse-yank-at-point t                              ; Mouse yank at point instead of click position.
 auto-window-vscroll nil                            ; This fix the cursor movement lag
 tab-width 4
 comment-empty-lines t
 visible-cursor t
 window-divider-default-right-width 1               ; Window divider setup
 window-divider-default-bottom-width 0
 window-divider-default-places t
 x-gtk-resize-child-frames nil
 x-underline-at-descent-line t
 bidi-inhibit-bpa t                                 ; Improve long line display performance
 bidi-paragraph-direction 'left-to-right
 echo-keystrokes 0.01                               ; don't wait for keystrokes display
 indent-tabs-mode nil                               ; indent with whitespace by default
 read-process-output-max (* 1024 1024)
 display-line-numbers-width 3                       ; Default line number width.
 pgtk-use-im-context-on-new-connection nil          ; Don't use Fcitx5 in Emacs in PGTK build
 warning-suppress-log-types '((comp))               ; Don't display compile warnings
 truncate-partial-width-windows 65                  ; Don't truncate lines in a window narrower than 65 chars.
 vc-follow-symlinks t                               ; always follow link
 scroll-step 1                                      ; Vertical Scroll
 scroll-margin 0
 scroll-conservatively 100000
 scroll-up-aggressively 0.01
 scroll-down-aggressively 0.01
 scroll-preserve-screen-position t
 auto-window-vscroll nil
 fast-but-imprecise-scrolling nil
 mouse-wheel-scroll-amount '(1 ((shift) . hscroll)) ; use shift + mouse wheel to scrll horizontally
 mouse-wheel-progressive-speed nil
 hscroll-step 1                                     ; Horizontal Scroll
 hscroll-margin 10
 server-client-instructions nil                     ; no client startup messages
 use-short-answers t                                ; yse-or-no -> y-or-n
 split-height-threshold nil                         ; prefer horizental split
 split-width-threshold 120
 suggest-key-bindings nil                           ; disable "You can run the command balabala..."
 )

(provide 'init-basic)
