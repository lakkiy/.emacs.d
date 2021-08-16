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
        eaf-kill-process-after-last-buffer-closed nil
        eaf-start-python-process-when-require t)
  :config
  (setq eaf-browser-enable-adblocker "true"))

(eat-package chess
  :straight t
  :init
  (setq chess-default-display '(chess-plain chess-ics1 chess-images)))

;; TODO custom
(eat-package telephone-line
  :straight t
  :config
  ;; Set mode-line height
  (setq telephone-line-height 24))

(provide 'init-fun)
