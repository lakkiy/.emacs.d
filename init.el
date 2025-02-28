;;; init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Load pre init
;;
(defun my-load-relative (file)
  "Load FILE relative to user-emacs-directory."
  (let ((rfile (expand-file-name file user-emacs-directory)))
    (when (file-exists-p rfile)
      (load rfile nil t t))))
(my-load-relative "pre-init.el")

;;; Load path
(setq custom-file (locate-user-emacs-file "custom.el"))
(setq custom-theme-directory (expand-file-name "themes" user-emacs-directory))
(push (expand-file-name "site-lisp" user-emacs-directory) load-path)

;;; Built-in config
(my-load-relative "lisp/init-builtin.el")

;;; package.el
;;
;; Install into separate package dirs for each Emacs version, to prevent
;; bytecode incompatibility
(setq package-user-dir (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                                         user-emacs-directory)
      package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
      package-refreshed nil
      package-quickstart t
      package-enable-at-startup nil)
(package-initialize)

(defun install-package (pkg &optional url)
  (unless (package-installed-p pkg)
    (unless package-refreshed
      (package-refresh-contents)
      (setq package-refreshed t))
    (if url
        (package-vc-install url)
      (package-install pkg))))

;;; Benchmark
;; (install-package 'benchmark-init)
;; (require 'benchmark-init)
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)
;; (benchmark-init/activate)

;;; Configs
(my-load-relative "lisp/init-meow.el")
(my-load-relative "lisp/init-ui.el")
(my-load-relative "lisp/init-lib.el")
(my-load-relative "lisp/init-tools.el")
(my-load-relative "lisp/init-window.el")
(my-load-relative "lisp/init-chinese.el")
(my-load-relative "lisp/init-completion.el")

(my-load-relative "lisp/init-dev.el")
(my-load-relative "lisp/init-lisp.el")
(my-load-relative "lisp/init-go.el")
(my-load-relative "lisp/init-python.el")

(my-load-relative "lisp/init-org.el")
(my-load-relative "lisp/init-git.el")
(my-load-relative "lisp/init-text.el")
(my-load-relative "lisp/init-mail.el")
(my-load-relative "lisp/init-shell.el")
(my-load-relative "lisp/init-telega.el")
(my-load-relative "lisp/init-dired.el")
(my-load-relative "lisp/init-translator.el")
(my-load-relative "lisp/init-ai.el")

(when (eq system-type 'darwin)
  (my-load-relative "lisp/init-osx.el"))

;;; Load post init
(my-load-relative "post-init.el")

;;; init.el ends here
