;;; -*- lexical-binding: t -*-

(eat-package smartparens
  :straight t
  :commands smartparens-mode
  :hook
  ((lisp-mode-hook lisp-interaction-hook emacs-lisp-mode-hook) . smartparens-mode)
  (smartparens-mode-hook . smartparens-strict-mode)
  :init
  (pretty-hydra-define smartparens-hydra (:title "smartparens" :quit-key "q")
    ("Moving"
     (("a" sp-beginning-of-sexp)
      ("e" sp-end-of-sexp)
      ("f" sp-forward-sexp)
      ("b" sp-backward-sexp)
      ("n" sp-down-sexp)
      ("N" sp-backward-down-sexp)
      ("p" sp-up-sexp)
      ("P" sp-backward-up-sexp))
     "Slurping & barfing"
     (("h" sp-backward-slurp-sexp)
      ("H" sp-backward-barf-sexp)
      ("l" sp-forward-slurp-sexp)
      ("L" sp-forward-barf-sexp))
     "Wrapping"
     (("R" sp-rewrap-sexp)
      ("u" sp-unwrap-sexp)
      ("U" sp-backward-unwrap-sexp)
      ("(" sp-wrap-round)
      ("{" sp-wrap-curly)
      ("[" sp-wrap-square))
     "Sexp juggling"
     (("S" sp-split-sexp)
      ("s" sp-splice-sexp)
      ("r" sp-raise-sexp)
      ("j" sp-join-sexp)
      ("t" sp-transpose-sexp)
      ("A" sp-absorb-sexp)
      ("E" sp-emit-sexp)
      ("o" sp-convolute-sexp))
     "Destructive editing"
     (("c" sp-change-inner :exit t)
      ("C" sp-change-enclosing :exit t)
      ("k" sp-kill-sexp)
      ("K" sp-backward-kill-sexp)
      ("w" sp-copy-sexp))))
  :config
  (require 'smartparens-config)
  (global-set-key (kbd "C-c C-p") 'smartparens-hydra/body))

(provide 'init-lisp)
