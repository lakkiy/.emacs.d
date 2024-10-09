;;; -*- lexical-binding: t -*-

;;; Init
;;
;; --debug-init implies `debug-on-error'.
(setq debug-on-error init-file-debug)
(setq custom-theme-directory (expand-file-name "themes" user-emacs-directory))
(push (expand-file-name "site-lisp" user-emacs-directory) load-path)

(defun my-load-relative (file)
  "Load FILE relative to user-emacs-directory."
  (let ((rfile (expand-file-name file user-emacs-directory)))
    (when (file-exists-p rfile)
      (load rfile nil t))))

;;; Custom file
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file) (load custom-file :no-error :no-message))

;;; Built-in config
(my-load-relative "lisp/init-var.el")
(my-load-relative "lisp/init-must.el")
(my-load-relative "lisp/init-utils.el")
(my-load-relative "lisp/init-font.el")
(my-load-relative "lisp/init-package.el")

;;; Benchmark
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
(my-load-relative "lisp/init-spell.el")
(my-load-relative "lisp/init-shell.el")
(my-load-relative "lisp/init-telega.el")
(my-load-relative "lisp/init-dired.el")
(my-load-relative "lisp/init-translator.el")
(my-load-relative "lisp/init-ai.el")

(when (eq system-type 'darwin)
  (my-load-relative "lisp/init-osx.el"))
