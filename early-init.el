(defvar +gc-cons-threshold gc-cons-threshold)

(defun +disable-gc () (setq gc-cons-threshold most-positive-fixnum))
(defun +enable-gc () (setq gc-cons-threshold +gc-cons-threshold))

(+disable-gc)

(add-hook 'emacs-startup-hook #'+enable-gc)
(add-hook 'minibuffer-setup-hook #'+disable-gc)
(add-hook 'minibuffer-exit-hook #'+enable-gc)

;; Faster to disable these here (before they've been initialized)
(push '(fullscreen . maximized) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(scroll-bar-mode . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

;; for now, just for vundo
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(setq package-enable-at-startup nil)

(require 'init-defaults)
(require 'init-straight)
(require 'init-themes)
