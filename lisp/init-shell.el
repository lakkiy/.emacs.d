;;; -*- lexical-binding: t -*-

;;; eshell
(add-hook 'eshell-mode-hook (lambda () (setq outline-regexp eshell-prompt-regexp)))

;;; eat
;;
;; https://abode.karthinks.com/share/eat-modes.png
(install-package 'eat)
(keymap-global-set "C-`" #'eat)

(setq eat-kill-buffer-on-exit t
      eat-enable-directory-tracking t)

(add-hook 'eshell-load-hook 'eat-eshell-mode)
(add-hook 'eshell-load-hook 'eat-eshell-visual-command-mode)

(defun jps-eat-term-name (&optional display)
  "Set terminal to xterm on remote hosts to ensure that backspace works https://codeberg.org/akib/emacs-eat/issues/119"
  (if (file-remote-p default-directory)
      "xterm-256color"
    (eat-term-get-suitable-term-name display)))
(setq eat-term-name 'jps-eat-term-name)

(provide 'init-shell)
