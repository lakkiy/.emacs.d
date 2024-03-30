;;; -*- lexical-binding: t -*-

;;; Init

;; --debug-init implies `debug-on-error'.
(setq debug-on-error init-file-debug)

(dolist (dir '("lisp" "lisp/lang" "site-lisp"))
  (push (expand-file-name dir user-emacs-directory) load-path))

(setq custom-theme-directory (expand-file-name "themes" user-emacs-directory))

(setq package-archives
      '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
	    ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
;; To prevent initializing twice
(setq package-enable-at-startup nil)
(package-initialize)

(defun install-package (pkg &optional url)
  (unless (package-installed-p pkg)
    (if url
        (package-vc-install url)
      (unless (assoc pkg package-archive-contents)
        (package-refresh-contents))
      (package-install pkg))))

;;; Benchmark
(install-package 'benchmark-init)
(add-hook 'after-init-hook 'benchmark-init/deactivate)
(benchmark-init/activate)

;;; Custom file
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file) (load custom-file :no-error :no-message))

;;; Configs
(require 'init-must)
(require 'init-utils)
(require 'init-font)

(require 'init-theme)
(require 'init-meow)
(require 'init-ui)
(require 'init-lib)
(require 'init-tools)
(require 'init-minibuffer)
(require 'init-window)
(require 'init-company)
(require 'init-dev)
(require 'init-rime)

(require 'init-org)
(require 'init-git)
(require 'init-text)
(require 'init-mail)
(require 'init-spell)
(require 'init-shell)
(require 'init-telega)
(require 'init-dirvish)

(when (eq system-type 'darwin)
  (require 'init-osx))
