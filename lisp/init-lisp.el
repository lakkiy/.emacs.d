;;; -*- lexical-binding: t -*-

(straight-use-package '(paredit :type git :host github :repo "emacsmirror/paredit"))

;; paredit
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)

(provide 'init-lisp)
