;;; init-shell.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;; eshell
(add-hook 'eshell-mode-hook (lambda () (setq outline-regexp eshell-prompt-regexp)))


;; eat
;;
;;                               ┌─────────────────────────────┐
;;                               │        Char Mode            │
;;                               │  (Full Terminal Emulation)  │
;;                               │                             │
;;                               │  • ALL keys to terminal     │
;;                               │  • No Emacs keys work       │
;;                               └─────────────────────────────┘
;;                               ▲ │           ▲               ▲
;;                               │ │           │               │
;;                       C-c M-d │ │ M-RET     │               │ C-c M-d
;;                               │ ▼           │               │
;;  ┌─────────────────────────────┐            │             ┌─────────────────────────────┐
;;  │      Semi-char Mode         │            │             │        Line Mode            │
;;  │        (Default)            │            │             │      (Comint-style)         │
;;  │                             │            │             │                             │
;;  │  All keys except:           │            │             │  • Line editing             │
;;  │  • C-\  • C-c  • C-x        │ ◀────────────────────▶   │  • History (↑↓)             │
;;  │  • C-g  • C-h  • C-M-c      │      C-c C-j / C-c C-l   │  • Tab completion           │
;;  │  • C-u  • M-x  • C-q        │            │             │  • Enter sends line         │
;;  │  • M-:  • M-!  • M-&        │            │             │                             │
;;  └─────────────────────────────┘    C-c M-d │             └─────────────────────────────┘
;;                              ▲ │            │              ▲ │
;;                      C-c C-j │ │ C-c C-e    │      C-c C-l │ │ C-c C-e
;;                              │ ▼            │              │ ▼
;;                               ┌─────────────────────────────┐
;;                               │        Emacs Mode           │
;;                               │    (Read-only buffer)       │
;;                               │                             │
;;                               │  • All Emacs keys work      │
;;                               │  • Buffer is read-only      │
;;                               │  • Free cursor movement     │
;;                               │  • Search with C-s/C-r      │
;;                               │  • Copy text only           │
;;                               └─────────────────────────────┘
(install-package 'eat)

(setq eat-term-name "xterm-256color")

(add-hook 'eshell-load-hook 'eat-eshell-mode)
(add-hook 'eshell-load-hook 'eat-eshell-visual-command-mode)

(defun lakki/eat-meow-setup ()
  (add-hook 'meow-normal-mode-hook 'eat-emacs-mode nil t)
  (add-hook 'meow-insert-mode-hook 'eat-char-mode nil t))

(defun lakki/eat-toggle-rime ()
  "切换输入法并自动切换 eat mode"
  (interactive)
  (if current-input-method
      (progn
        (deactivate-input-method)
        (eat-char-mode))
    (progn
      (activate-input-method "rime")
      (eat-line-mode))))

(with-eval-after-load "eat"
  (define-key eat-char-mode-map (kbd "C-y") 'eat-yank)
  (define-key eat-char-mode-map (kbd "C-\\") 'lakki/eat-toggle-rime)
  (define-key eat-line-mode-map (kbd "C-\\") 'lakki/eat-toggle-rime)
  (advice-add 'eat-semi-char-mode :after 'eat-emacs-mode)
  (add-hook 'eat-mode-hook 'lakki/eat-meow-setup))

;;; init-shell.el ends here
