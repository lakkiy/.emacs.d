;;; -*- lexical-binding: t -*-

;; eshell
(add-hook 'eshell-mode-hook (lambda () (setq outline-regexp eshell-prompt-regexp)))


;; eat
;;
;; https://abode.karthinks.com/share/eat-modes.png
(install-package 'eat)

(defun jps-eat-term-name (&optional display)
  "Set terminal to xterm on remote hosts to ensure that backspace works.
https://codeberg.org/akib/emacs-eat/issues/119 "
  (if (file-remote-p default-directory)
      "xterm-256color"
    (eat-term-get-suitable-term-name display)))

(setq eat-term-name 'jps-eat-term-name)
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


(provide 'init-shell)
;;; init-shell.el ends here
