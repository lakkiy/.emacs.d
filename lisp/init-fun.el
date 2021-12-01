;;; -*- lexical-binding: t -*-

(eat-package emacs-application-framework
  :straight (emacs-application-framework
             :type git
             :host github
             :repo "emacs-eaf/emacs-application-framework"
             :files ("*.el" "*.py" "core" "app"))
  :init
  (eat-package s :straight t)
  (eat-package ctable :straight t)
  (eat-package epc :straight t)
  (eat-package deferred :straight t)
  ;; TODO
  ;; (require 'eaf)

  (eat-package eaf-browser
    :straight (eaf-browser :type git :host github :repo "emacs-eaf/eaf-browser" :files ("*.el" "*.py"))
    :init
    (setq eaf-browser-continue-where-left-off t
          eaf-browser-enable-autofill t
          eaf-browser-enable-adblocker t)))

;; install pyqt5 with your package manager(not pip)
(eat-package popweb
  :straight (popweb
             :type git
             :host github
             :repo "manateelazycat/popweb"
             :files ("*.el" "*.py" "*.js" "*.html" "*.css" "extension")))

(eat-package chess :straight t)

(eat-package rainbow-mode
  :straight t
  :commands rainbow-mode)

(eat-package secret-mode
  :straight (secret-mode :type git :host github :repo  "bkaestner/secret-mode.el"))

(eat-package screenshot
  :straight (screenshot :type git :host github :repo "tecosaur/screenshot"))

(eat-package xterm-color
  :straight t
  :init
  ;; For shell and interpreters
  (setenv "TERM" "xterm-256color")
  ;; For compilation buffers
  (setq compilation-environment '("TERM=xterm-256color")))

(eat-package vterm :straight t)

(eat-package popper
  :straight t
  :init
  (setq popper-mode-line nil
        popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          "^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
          "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
          "^\\*term.*\\*$"   term-mode   ;term as a popup
          "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
          compilation-mode))
  (popper-mode +1)
  (when (not (display-graphic-p))
    (global-set-key (kbd "M-`") #'popper-toggle-latest)))

(eat-package which-key
  :straight t
  :init
  ;; Allow C-h to trigger which-key before it is done automatically
  (setq which-key-show-early-on-C-h t)
  ;; make sure which-key doesn't show normally but refreshes quickly after it is
  ;; triggered.
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05))

(eat-package citar
  :straight t
  :init
  (setq citar-bibliography '("~/Dropbox/bib/references.bib")))

(eat-package find-orphan
  :straight (find-orphan
             :type git
             :host github
             :repo "manateelazycat/find-orphan")
  :commands
  find-orphan-function-in-buffer
  find-orphan-function-in-directory)

(provide 'init-fun)
