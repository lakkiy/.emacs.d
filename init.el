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

;;; Load post init
(my-load-relative "post-init.el")

(provide 'init)
;;; init.el ends here
