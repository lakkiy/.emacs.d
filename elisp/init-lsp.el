;;; -*- lexical-binding: t -*-

(use-package flymake
  :straight (:type built-in)
  :commands (flymake-mode)
  :bind
  (:map flymake-mode-map
        ("M-n" . 'flymake-goto-next-error)
        ("M-p" . 'flymake-goto-prev-error)))

(use-package eglot
  :commands (eglot-ensure eglot)
  :hook
  ((go-mode python-mode rust-mode) . eglot-ensure)
  :custom
  (eglot-stay-out-of '())
  (eglot-ignored-server-capabilites '(:documentHighlightProvider))
  :config
  (add-to-list 'eglot-server-programs
			   '(python-mode "pyright"))
  (add-to-list 'eglot-server-programs
			   '(rust-mode "rust-analyzer")))

(provide 'init-lsp)
