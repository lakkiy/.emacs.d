(defvar +gc-cons-threshold gc-cons-threshold)

(defun +disable-gc () (setq gc-cons-threshold most-positive-fixnum))
(defun +enable-gc () (setq gc-cons-threshold +gc-cons-threshold))

(add-hook 'after-init-hook #'+enable-gc)
(add-hook 'minibuffer-setup-hook #'+disable-gc)
(add-hook 'minibuffer-exit-hook #'+enable-gc)
(+disable-gc)

(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

(require 'init-defaults)
(require 'init-straight)
