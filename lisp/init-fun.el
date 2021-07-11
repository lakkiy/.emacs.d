;;; -*- lexical-binding: t -*-

(eat-package twidget
  :straight (twidget :type git :host github :repo "Kinneyzhang/twidget")
  :init
  ;; FIXME :straight should install multi packages
  (straight-use-package 'ov))

(eat-package svg-lib
  :straight (svg-lib :type git :host github :repo "rougier/svg-lib"))

(eat-package netease-cloud-music
  :straight (netease-cloud-music
             :type git
             :host github
             :repo "SpringHan/netease-cloud-music.el"))

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

(provide 'init-fun)
