;;; init-shell.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;; eshell
(add-hook 'eshell-mode-hook (lambda () (setq outline-regexp eshell-prompt-regexp)))


;; eat
;;
;; https://abode.karthinks.com/share/eat-modes.png
(install-package 'eat)

;; https://codeberg.org/akib/emacs-eat/issues/119
(setq eat-term-name "xterm-256color")
(setq eat-kill-buffer-on-exit t
      eat-enable-directory-tracking t)

(add-hook 'eshell-load-hook 'eat-eshell-mode)
(add-hook 'eshell-load-hook 'eat-eshell-visual-command-mode)

;; Use char mode in INSERT state, and emacs mode in NORMAL
;; state. When switching to INSERT state, move the cursor to the end
;; of buffer.
(defun eat-meow-setup ()
  (add-hook 'meow-normal-mode-hook 'eat-emacs-mode nil t)
  (add-hook 'meow-insert-mode-hook 'eat-char-mode nil t))

(with-eval-after-load "eat"
  (define-key eat-char-mode-map (kbd "C-y") 'eat-yank)
  ;; Replace semi-char mode with emacs mode
  (advice-add 'eat-semi-char-mode :after 'eat-emacs-mode)
  (add-hook 'eat-mode-hook 'eat-meow-setup))

;;; init-shell.el ends here
