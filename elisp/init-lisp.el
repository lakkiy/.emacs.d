;;; -*- lexical-binding: t -*-

(use-package paredit
  :straight
  (paredit :type git
           :host github
           :repo "emacsmirror/paredit")
  :hook
  (emacs-lisp-mode . paredit-mode)
  (lisp-mode . paredit-mode))

(provide 'init-lisp)
