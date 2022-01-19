;;; -*- lexical-binding: t -*-
;; TODO:
;; Maybe add some config about gc.

;; UI
(unless (eq window-system 'ns)
  (menu-bar-mode -1))
(when (display-graphic-p)
  (tool-bar-mode 0)
  (scroll-bar-mode 0))
(setq inhibit-startup-screen t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Basic and built-in package config
(let ((file-name-handler-alist nil))
  ;; (require 'init-straight)
  (require 'init-basic)
  (require 'init-builtin)
  ;; (require 'init-dired)
  )
