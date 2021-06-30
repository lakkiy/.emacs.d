;;; -*- lexical-binding: t -*-

(eat-package twidget
  :straight (twidget :type git :host github :repo "Kinneyzhang/twidget")
  :init
  ;; FIXME :straight should install multi packages
  (straight-use-package 'ov))

(eat-package svg-lib
  :straight (svg-lib :type git :host github :repo "rougier/svg-lib"))

(provide 'init-fun)
