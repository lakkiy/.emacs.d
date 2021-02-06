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
 ;; Improve long line display performance
 bidi-inhibit-bpa t
 bidi-paragraph-direction 'left-to-right
 indent-tabs-mode nil
 read-process-output-max (* 1024 1024)
 ;; Default line number width.
 display-line-numbers-width 4
 ;; Don't display comp warnings
 warning-suppress-log-types '((comp)))

(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'conf-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'subword-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(add-hook 'after-init-hook (lambda () (blink-cursor-mode -1)))

;;; project.el use C-x p
(global-unset-key (kbd "C-x C-p"))
(global-set-key (kbd "C-x C-d") #'dired)

(defun +reopen-file-with-sudo ()
  (interactive)
  (find-alternate-file (format "/sudo::%s" (buffer-file-name))))

(global-set-key (kbd "C-x C-z") #'+reopen-file-with-sudo)
;; (global-set-key (kbd "<f7>") #'profiler-start)
;; (global-set-key (kbd "<f8>") #'profiler-report)

(provide 'init-defaults)
