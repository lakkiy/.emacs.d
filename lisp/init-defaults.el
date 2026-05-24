;;; init-defaults.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Disable stupid things
;;
;; Reduce *Message* noise at startup. An empty scratch buffer (or the
;; dashboard) is more than enough, and faster to display.
(setq inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-buffer-menu t
      inhibit-startup-echo-area-message user-login-name
      inhibit-x-resources t
      inhibit-default-init t
      server-client-instructions nil
      suggest-key-bindings nil
      x-gtk-use-system-tooltips nil
      x-gtk-use-native-input t
      x-gtk-resize-child-frames 'resize-mode)

;; Remove "For information about GNU Emacs..." message at startup
(fset #'display-startup-echo-area-message #'ignore)

;; Disable warnings from the legacy advice API. They aren't useful.
(setq ad-redefinition-action 'accept)

;; Ignore warnings about "existing variables being aliased".
(setq warning-suppress-types '((defvaralias) (lexical-binding)))

;; Disable GUIs because they are inconsistent across systems, desktop
;; environments, and themes, and they don't match the look of Emacs.
(setq use-file-dialog nil
      use-dialog-box nil)

;; No annoying bell
(setq ring-bell-function 'ignore)

;; No eyes distraction
(setq blink-cursor-mode nil)

;; Allow for shorter responses: "y" for yes and "n" for no.
(setq use-short-answers t
      read-answer-short t
      ;; 直接读取单个字符（如 y 或 n），不需要 enter 确认
      y-or-n-p-use-read-key t
      read-char-choice-use-read-key t)

;; Never show the hello file
(keymap-global-unset "C-h h")

;; Do not hide emacs
(keymap-global-unset "M-z")
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;;; Better default
;;
;; By default, Emacs "updates" its ui more often than it needs to
(setq idle-update-delay 1.0)

;; Ensures that empty lines within the commented region are also commented out.
;; This prevents unintended visual gaps and maintains a consistent appearance,
;; ensuring that comments apply uniformly to all lines, including those that are
;; otherwise empty.
(setq comment-empty-lines t)

;; We often split terminals and editor windows or place them side-by-side,
;; making use of the additional horizontal space.
(setq-default fill-column 80)

;; Position underlines at the descent line instead of the baseline.
(setq x-underline-at-descent-line t)

;; No Fcitx5 in Emacs PGTK build.
(setq pgtk-use-im-context-on-new-connection nil)

;; Better word wrapping for CJK characters
(setq word-wrap-by-category t)

;; Disable the obsolete practice of end-of-line spacing from the
;; typewriter era.
(setq sentence-end-double-space nil)

;; Echo current unfinished command immediately.
(setq echo-keystrokes 0.1)

;; Allow nested minibuffers
(setq enable-recursive-minibuffers t)

;; Keep the cursor out of the read-only portions of the.minibuffer
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Disable truncation of printed s-expressions in the message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;; Delete by moving to trash in interactive mode
(setq delete-by-moving-to-trash (not noninteractive))

;; Disable the warning "X and Y are the same file". Ignoring this warning is
;; acceptable since it will redirect you to the existing buffer regardless.
(setq find-file-suppress-same-file-warnings t)

;; Resolve symlinks when opening files, so that any operations are conducted
;; from the file's true directory (like `find-file').
(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; According to the POSIX, a line is defined as "a sequence of zero or
;; more non-newline characters followed by a terminating newline".
(setq require-final-newline t)

;; https://emacsredux.com/blog/2025/06/01/let-s-make-keyboard-quit-smarter
(defun lakki.is/keyboard-quit ()
  "Smater version of the built-in `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it."
  (interactive)
  (if (active-minibuffer-window)
      (if (minibufferp)
          (minibuffer-keyboard-quit)
        (abort-recursive-edit))
    (keyboard-quit)))
(global-set-key [remap keyboard-quit] 'lakki.is/keyboard-quit)

(setq native-comp-async-report-warnings-errors 'silent)

;;; Performance
;;
;; Improve Emacs' responsiveness by delaying syntax highlighting during input
;; but may reduce visual feedback.
(setq redisplay-skip-fontification-on-input t)

;; Resizing the Emacs frame can be costly when changing the font. Disable this
;; to improve startup times with fonts larger than the system default.
(setq frame-resize-pixelwise t)
;; However, do not resize windows pixelwise, as this can cause crashes in some
;; cases when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)

;; Shave seconds off startup time by starting the scratch buffer in
;; `fundamental-mode'
(setq initial-buffer-choice nil
      initial-major-mode 'fundamental-mode)

;; Increase how much is read from processes in a single chunk (default is 4kb).
(setq read-process-output-max (* 4 1024 1024)
      process-adaptive-read-buffering nil)

;; Reduce rendering/line scan work by not rendering cursors or regions in
;; non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
;; No second pass of case-insensitive search over auto-mode-alist.
(setq auto-mode-case-fold nil)

;; Hide commands in M-x which do not work in the current mode, reduce command
;; completion overhead.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Long line
;;
;; https://emacs-china.org/t/topic/25811/9?u=rua
(setq long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)
(add-hook 'after-init-hook #'global-so-long-mode)

;; Disable bidirectional text scanning for a modest performance boost.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Give up some bidirectional functionality for slightly faster re-display.
(setq bidi-inhibit-bpa t)

;;; init-defaults.el ends here
