;;; -*- lexical-binding: t -*-

(eat-package rainbow-mode
  :straight t
  :commands rainbow-mode)

;; NOTE not work on macos with emacs-build
(eat-package screenshot
  :straight (screenshot :type git :host github :repo "tecosaur/screenshot"))

(eat-package find-orphan
  :straight (find-orphan
             :type git
             :host github
             :repo "manateelazycat/find-orphan")
  :commands
  find-orphan-function-in-buffer
  find-orphan-function-in-directory)

(eat-package vterm :straight t)
(eat-package eldoc-overlay :straight t)
(eat-package devdocs :straight t)

(provide 'init-fun)
