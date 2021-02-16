;;; -*- lexical-binding: t -*-

(leaf paredit
  :straight
  (paredit :type git :host github :repo "emacsmirror/paredit")
  :hook
  ((emacs-lisp-mode-hook lisp-mode-hook) . paredit-mode))

(provide 'init-lisp)
