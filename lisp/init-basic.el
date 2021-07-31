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
(defvar +font-variable-pitch "Bookerly" "Font use in variable-pitch-mode.")
(defvar +font-height (cond (sys/macp 130) (t 110)))
(defvar +use-header-line nil "Wheather to use header line.")
(defvar +theme 'doom-spacegrey "Theme use in gui.")
(defvar +theme-tui 'minidark "Theme use in tui.")
(defvar +theme-system-light 'doom-solarized-light "Theme used after change system apperance to light.")
(defvar +theme-system-dark 'doom-solarized-dark "Theme used after change system apperance to dark.")
(defvar +enable-proxy? nil)
(defvar +proxy "127.0.0.1:7890")
(defvar +erc-password "")
(defvar +telega-proxy nil)

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
(when (and (boundp 'ns-system-appearance) (display-graphic-p))
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

(eat-package ibuffer
  :init
  ;; ibuffer
  (global-unset-key (kbd "C-x C-b"))
  (global-set-key (kbd "C-x C-b") 'ibuffer))

(eat-package display-line-numbers
  ;; :hook ((prog-mode-hook conf-mode-hook) . display-line-numbers-mode)
  )

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
  :hook (after-init-hook . electric-pair-mode)
  :init
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(eat-package saveplace
  :hook (after-init-hook . save-place-mode))

(eat-package paren
  :hook (after-init-hook . show-paren-mode)
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

(setq-default
 ;; Close up of MacOs
 ring-bell-function 'ignore
 ;; no start messages
 inhibit-startup-message t
 ;; don't read x resource file
 inhibit-x-resources t
 ;; no welcome screen
 inhibit-splash-screen t
 inhibit-startup-screen t
 ;; no startup messages
 inhibit-startup-echo-area-message t
 frame-inhibit-implied-resize t
 initial-scratch-message ""
 hl-line-sticky-flag nil
 ;; Don't create lockfiles
 create-lockfiles nil
 ;; UTF-8
 buffer-file-coding-system 'utf-8-unix
 default-file-name-coding-system 'utf-8-unix
 default-keyboard-coding-system 'utf-8-unix
 default-process-coding-system '(utf-8-unix . utf-8-unix)
 default-sendmail-coding-system 'utf-8-unix
 default-terminal-coding-system 'utf-8-unix
 ;; add final newline
 require-final-newline t
 ;; Disable auto save and backup
 make-backup-files nil
 auto-save-default nil
 auto-save-list-file-prefix nil
 ;; Mouse yank at point instead of click position.
 mouse-yank-at-point t
 ;; This fix the cursor movement lag
 auto-window-vscroll nil
 tab-width 4
 ;; Don't show cursor in non selected window.
 cursor-in-non-selected-windows nil
 comment-empty-lines t
 visible-cursor t
 ;; Window divider setup
 window-divider-default-right-width 1
 window-divider-default-bottom-width 0
 window-divider-default-places t
 ;; allow resize by pixels
 frame-resize-pixelwise t
 x-gtk-resize-child-frames nil
 x-underline-at-descent-line t
 ;; Improve long line display performance
 bidi-inhibit-bpa t
 bidi-paragraph-direction 'left-to-right
 ;; don't wait for keystrokes display
 echo-keystrokes 0.01
 ;; indent with whitespace by default
 indent-tabs-mode nil
 read-process-output-max (* 1024 1024)
 ;; Default line number width.
 display-line-numbers-width 3
 ;; Don't use Fcitx5 in Emacs in PGTK build
 pgtk-use-im-context-on-new-connection nil
 ;; Don't display compile warnings
 warning-suppress-log-types '((comp))
 ;; Don't truncate lines in a window narrower than 65 chars.
 truncate-partial-width-windows 65
 ;; always follow link
 vc-follow-symlinks t
 ;; Vertical Scroll
 scroll-step 1
 scroll-margin 0
 scroll-conservatively 100000
 scroll-up-aggressively 0.01
 scroll-down-aggressively 0.01
 scroll-preserve-screen-position t
 auto-window-vscroll nil
 fast-but-imprecise-scrolling nil
 ;; use shift + mouse wheel to scrll horizontally
 mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
 mouse-wheel-progressive-speed nil
 ;; Horizontal Scroll
 hscroll-step 1
 hscroll-margin 10
 ;; no client startup messages
 server-client-instructions nil
 ;; install hunspell and hunspell-en_US
 ;; yse-or-no -> y-or-n
 use-short-answers t
 ;; prefer horizental split
 split-height-threshold nil
 split-width-threshold 120
 ;; disable "You can run the command balabala..."
 suggest-key-bindings nil)

(eat-package ispell
  :doc "install hunspell and hunspell-en_US on your system to use"
  :init
  (setq  ispell-dictionary "en_US"
         ispell-program-name "hunspell"
         ispell-personal-dictionary (expand-file-name ".hunspell_dict.txt" user-emacs-directory)
         ))

(provide 'init-basic)
