;;; -*- lexical-binding: t -*-

(setq package-archives
      '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
	    ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(setq package-enable-at-startup nil)
(package-initialize)

(defun install-package (pkg &optional url)
  (unless (package-installed-p pkg)
    (if url
        (package-vc-install url)
      (unless (assoc pkg package-archive-contents)
        (package-refresh-contents))
      (package-install pkg))))

;; Show startup time.
(defun my/show-startup-time ()
  "Print startup time."
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))
(add-hook 'emacs-startup-hook #'my/show-startup-time)

;; --debug-init implies `debug-on-error'.
(setq debug-on-error init-file-debug)

;; Setup `load-path'.
(defun dir-concat (dir file)
  "join path DIR with filename FILE correctly"
  (concat (file-name-as-directory dir) file))
(push (dir-concat user-emacs-directory "lisp/") load-path)
(push (dir-concat user-emacs-directory "lisp/lang/") load-path)
(push (dir-concat user-emacs-directory "site-lisp/") load-path)
(push (dir-concat user-emacs-directory "themes/") custom-theme-load-path)

(setq custom-file (locate-user-emacs-file "custom.el"))

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

(when (and (file-exists-p custom-file))
  (load custom-file :no-error :no-message))
