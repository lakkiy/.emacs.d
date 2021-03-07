;;; -*- lexical-binding: t -*-

(leaf flymake
  :tag "builtin"
  :commands flymake-mode
  :bind ((flymake-mode-map
          ("M-n" . flymake-goto-next-error)
          ("M-p" . flymake-goto-prev-error))))

(leaf eglot
  :straight t
  :commands (eglot-ensure eglot)
  :hook
  ((go-mode-hook python-mode-hook rust-mode-hook) . eglot-ensure)
  :custom
  (eglot-stay-out-of . '())
  (eglot-ignored-server-capabilites . '(:documentHighlightProvider))
  :config
  (add-to-list 'eglot-server-programs
			   '(rust-mode "rust-analyzer")))

(provide 'init-lsp)
