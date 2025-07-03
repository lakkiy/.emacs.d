;;; init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Load pre init
;;
(defun lakki.is/load-relative (file)
  "Load FILE relative to user-emacs-directory."
  (let ((rfile (expand-file-name file user-emacs-directory)))
    (when (file-exists-p rfile)
      (load rfile nil t t))))
(lakki.is/load-relative "before-init.el")

;;; Load path
(setq custom-file (locate-user-emacs-file "custom.el"))
(setq custom-theme-directory (expand-file-name "themes" user-emacs-directory))
(push (expand-file-name "site-lisp" user-emacs-directory) load-path)

;;; Built-in config
(lakki.is/load-relative "lisp/init-builtin.el")

;;; package.el
;;
;; Install into separate package dirs for each Emacs version, to prevent
;; bytecode incompatibility
(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")))
(setq package-user-dir (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version) user-emacs-directory))
(setq package-enable-at-startup nil)
(package-initialize)

(defvar package-refreshed nil)
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
(lakki.is/load-relative "lisp/init-meow.el")
(lakki.is/load-relative "lisp/init-ui.el")
(lakki.is/load-relative "lisp/init-lib.el")
(lakki.is/load-relative "lisp/init-tools.el")
(lakki.is/load-relative "lisp/init-window.el")
(lakki.is/load-relative "lisp/init-chinese.el")
(lakki.is/load-relative "lisp/init-completion.el")

(lakki.is/load-relative "lisp/init-dev.el")
(lakki.is/load-relative "lisp/init-lisp.el")
(lakki.is/load-relative "lisp/init-go.el")
(lakki.is/load-relative "lisp/init-python.el")
(lakki.is/load-relative "lisp/init-web.el")

(lakki.is/load-relative "lisp/init-org.el")
(lakki.is/load-relative "lisp/init-git.el")
(lakki.is/load-relative "lisp/init-text.el")
(lakki.is/load-relative "lisp/init-mail.el")
(lakki.is/load-relative "lisp/init-shell.el")
(lakki.is/load-relative "lisp/init-telega.el")
(lakki.is/load-relative "lisp/init-dired.el")
(lakki.is/load-relative "lisp/init-translator.el")
(lakki.is/load-relative "lisp/init-ai.el")

(when (eq system-type 'darwin)
  (lakki.is/load-relative "lisp/init-osx.el"))

;;; Load post init
(lakki.is/load-relative "after-init.el")

;;; init.el ends here
