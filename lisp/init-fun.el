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

(eat-package chess :straight t)

(eat-package toki-term
  :commands
  ;; TODO seems not work
  ;; TODO remap `term' to `toki-term'
  toki-term
  :config
  (toki-term-setup-escape-keys))

(eat-package rainbow-mode
  :straight t
  :commands rainbow-mode)

(eat-package secret-mode
  :straight (secret-mode :type git :host github :repo  "bkaestner/secret-mode.el"))

(eat-package screenshot
  :straight (screenshot :type git :host github :repo "tecosaur/screenshot"))

(provide 'init-fun)
