;;; -*- lexical-binding: t -*-

;; Basic and built-in package config
(let ((file-name-handler-alist nil))
  (load (locate-user-emacs-file "early-init.el"))
  (require 'init-straight)
  (require 'init-font)
  (require 'init-themes))
