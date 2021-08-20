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
  (setq eaf-browser-continue-where-left-off t
        eaf-browser-enable-autofill t
        eaf-browser-enable-adblocker t))

(eat-package chess :straight t)

(eat-package toki-term
  :commands
  ;; TODO seems not work
  ;; TODO remap `term' to `toki-term'
  toki-term
  :config
  (toki-term-setup-escape-keys))

(provide 'init-fun)
