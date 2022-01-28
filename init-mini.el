;;; -*- lexical-binding: t -*-

;; Basic and built-in package config
(let ((file-name-handler-alist nil))
  (load (locate-user-emacs-file "early-init.el") :no-message)
  (require 'init-straight))
