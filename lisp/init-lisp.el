;;; -*- lexical-binding: t -*-

(straight-use-package 'smartparens)
(straight-use-package 'clojure-mode)
(straight-use-package 'cider)
(straight-use-package 'flycheck)
(straight-use-package 'flycheck-clj-kondo)

;;; flycheck
(with-eval-after-load "flycheck"
  (define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error)
  (setq flycheck-idle-change-delay 1))

;;; smartparens
(autoload #'smartparens-mode "smartparens" nil t)

(add-hook 'lisp-mode-hook 'smartparens-mode)
(add-hook 'lisp-interaction-hook 'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
(add-hook 'clojure-mode-hook 'smartparens-mode)
(add-hook 'smartparens-mode-hook 'smartparens-strict-mode)

(with-eval-after-load "smartparens"
  (require 'smartparens-config))

;;; clojure-mode
(autoload 'clojure-mode "clojure-mode")

(with-eval-after-load "clojure"
  (require 'smartparens-clojure)
  (add-hook 'clojure-mode-hook 'flycheck-mode)
  (require 'flycheck-clj-kondo))

;;; cider
(autoload #'cider-jack-in "cider" nil t)
(autoload #'cider-jack-in-cljs "cider" nil t)
(autoload #'cider-jack-in-clj&cljs "cider" nil t)
(autoload #'cider "cider" nil t)

(provide 'init-lisp)
