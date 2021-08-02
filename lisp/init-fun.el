;;; -*- lexical-binding: t -*-

(eat-package emacs-application-framework
  :straight (emacs-application-framework
             :type git
             :host github
             :repo "manateelazycat/emacs-application-framework"
             :files ("*.el" "*.py" "core" "app"))
  :init
  (eat-package s :straight t)
  (eat-package ctable :straight t)
  (eat-package epc :straight t)
  (eat-package deferred :straight t)
  (setq eaf-browser-continue-where-left-off t)
  :config
  (setq eaf-browser-enable-adblocker "true"))

(eat-package simple-call-tree :straight t)

(eat-package chess
  :straight t
  :init
  (setq chess-default-display '(chess-plain chess-ics1 chess-images)))

(provide 'init-fun)
