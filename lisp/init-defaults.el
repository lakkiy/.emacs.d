;;; -*- lexical-binding: t -*-

(setq-default
 ring-bell-function 'ignore
 inhibit-startup-message t
 inhibit-splash-screen t
 inhibit-startup-screen t
 inhibit-startup-echo-area-message t
 initial-major-mode 'emacs-lisp-mode
 initial-scratch-message ""
 ;; Don't create lockfiles
 create-lockfiles nil
 ;; UTF-8
 buffer-file-coding-system 'utf-8-unix
 default-file-name-coding-system 'utf-8-unix
 default-keyboard-coding-system 'utf-8-unix
 default-process-coding-system '(utf-8-unix . utf-8-unix)
 default-sendmail-coding-system 'utf-8-unix
 default-terminal-coding-system 'utf-8-unix
 ;; Disable auto save and backup
 make-backup-files nil
 auto-save-default nil
 auto-save-list-file-prefix nil
 ;; Xref no prompt
 xref-prompt-for-identifier nil
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
 window-divider-default-bottom-width 1
 window-divider-default-places t
 ;; Improve long line display performance
 bidi-inhibit-bpa t
 bidi-paragraph-direction 'left-to-right
 indent-tabs-mode nil
 read-process-output-max (* 1024 1024)
 ;; Default line number width.
 display-line-numbers-width 4
 ;; Disable title bar text and icon for macos
 ns-use-proxy-icon  nil
 frame-title-format nil
 ;; Don't display compile warnings
 warning-suppress-log-types '((comp)))

(setq
 custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'init-defaults)
