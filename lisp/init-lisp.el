;;; -*- lexical-binding: t -*-

(eat-package smartparens
  :straight t
  :commands smartparens-mode
  :hook
  ((lisp-mode-hook lisp-interaction-hook emacs-lisp-mode-hook) . smartparens-mode)
  (smartparens-mode-hook . smartparens-strict-mode)
  :config
  (require 'smartparens-config)
  (define-key smartparens-mode-map (kbd "M-r") #'sp-raise-sexp)
  (define-key smartparens-mode-map (kbd "M-s") #'sp-unwrap-sexp)
  (define-key smartparens-mode-map (kbd "M-[") #'sp-wrap-square)
  (define-key smartparens-mode-map (kbd "M-{") #'sp-wrap-curly)
  (define-key smartparens-mode-map (kbd "M-(") #'sp-wrap-round)
  (define-key smartparens-mode-map (kbd "C-)") #'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-}") #'sp-forward-barf-sexp))

(provide 'init-lisp)
