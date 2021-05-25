;;; -*- lexical-binding: t -*-

(straight-use-package 'mini-frame)

(defun my/load-nano ()
  (interactive)
  (require 'nano-theme-light)
  ;; (require 'nano-theme-dark)
  (require 'nano-faces)
  (nano-faces)
  (require 'nano-theme)
  (nano-theme)
  (require 'nano-modeline))

(provide 'init-nano)
