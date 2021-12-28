;;; -*- lexical-binding: t -*-
;; TODO:
;; Maybe add some config about gc.

;; Disable menu-bar
(unless (eq window-system 'ns)
  (menu-bar-mode -1))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(let ((file-name-handler-alist nil))
  (require 'init-basic)
  (require 'init-builtin))
