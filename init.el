;;; -*- lexical-binding: t -*-

;; --debug-init implies `debug-on-error'.
(setq debug-on-error init-file-debug)

(dolist (dir '("lisp" "lisp/lang" "site-lisp"))
  (push (expand-file-name dir user-emacs-directory) load-path))

(setq custom-theme-directory (expand-file-name "themes" user-emacs-directory))

;; Custom file
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file) (load custom-file :no-error :no-message))

(require 'init-must)
(require 'init-utils)
(require 'init-font)

(require 'init-package)
;; (require 'init-benchmark)

(require 'init-meow)
(require 'init-ui)
(require 'init-lib)
(require 'init-tools)
(require 'init-minibuffer)
(require 'init-window)
(require 'init-company)
(require 'init-rime)

(require 'init-dev)
(require 'init-lisp)
(require 'init-go)
(require 'init-python)
(require 'init-c)
(require 'init-nix)
(require 'init-web)

;; standalone apps
(require 'init-org)
(require 'init-git)
(require 'init-text)
(require 'init-mail)
(require 'init-spell)
(require 'init-shell)
(require 'init-telega)
(require 'init-dired)
(require 'init-translator)
(require 'init-ai)

(when (eq system-type 'darwin)
  (require 'init-osx))
