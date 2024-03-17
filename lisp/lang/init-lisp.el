;;; -*- lexical-binding: t -*-

(setq-default lexical-binding t)

(install-package 'paredit)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'paredit-mode)
(add-hook 'scheme-mode-hook #'paredit-mode)
(add-hook 'lisp-mode-hook #'paredit-mode)

(install-package 'aggressive-indent)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'lisp-interaction-mode-hook #'aggressive-indent-mode)
(add-hook 'scheme-mode-hook #'aggressive-indent-mode)
(add-hook 'lisp-mode-hook #'aggressive-indent-mode)

(install-package 'sly)
(setq inferior-lisp-program "sbcl")

(install-package 'clojure-mode)
(install-package 'cider)
(install-package 'clj-refactor)

(add-hook 'clojure-mode-hook #'puni-mode)

(with-eval-after-load 'clojure-mode
  ;; better indentation for compojure
  ;; https://github.com/weavejester/compojure/wiki/Emacs-indentation
  (define-clojure-indent
   (defroutes 'defun)
   (GET 2)
   (POST 2)
   (PUT 2)
   (DELETE 2)
   (HEAD 2)
   (ANY 2)
   (OPTIONS 2)
   (PATCH 2)
   (rfn 2)
   (let-routes 1)
   (context 2)))

(provide 'init-lisp)
