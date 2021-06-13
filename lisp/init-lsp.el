;;; -*- lexical-binding: t -*-

(straight-use-package 'flymake)
(straight-use-package 'eglot)
(straight-use-package 'docstr)
(straight-use-package '(devdocs :type git :host github :repo "astoff/devdocs.el"))
(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)

(+pdump-packages 'flymake
                 'docstr
                 'devdocs
                 'tree-sitter
                 'tree-sitter-langs
                 'eglot)

;; pulse current line
(defun pulse-region (beg end &rest _)
  "Pulse the current region."
  (pulse-momentary-highlight-region beg end))
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))
(defun recenter-and-pulse (&rest _)
  "Recenter and pulse the current line."
  (recenter)
  (pulse-line))

(advice-add #'xref-find-definitions :after #'recenter-and-pulse)
(advice-add #'xref-find-definitions-at-mouse :after #'recenter-and-pulse)
(advice-add #'xref-pop-marker-stack :after #'recenter-and-pulse)
(add-hook 'dump-jump-after-jump-hook 'recenter-and-pulse)
(add-hook 'bookmark-after-jump-hook 'recenter-and-pulse)
(add-hook 'imenu-after-jump-hook 'recenter-and-pulse)

;;; tree-sitter
;; (add-hook 'go-mode-hook 'tree-sitter-mode)
;; (add-hook 'go-mode-hook 'tree-sitter-hl-mode)

(with-eval-after-load "tree-sitter"
  (require 'tree-sitter-langs))

;;; devdocs
(global-set-key (kbd "C-c b") 'devdocs-lookup)

;; docstr
;; FIXME not work with meow in go mode
(add-hook 'prog-mode-hook (lambda () (docstr-mode 1)))

;;; flymake
(autoload #'flymake-mode "flymake" nil t)

(with-eval-after-load "flymake"
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

;;; eglot
(setq
 eglot-stay-out-of nil
 eglot-ignored-server-capabilites '(:documentHighlightProvider))

(autoload #'eglot-ensure "eglot" nil t)
(autoload #'eglot "eglot" nil t)

(with-eval-after-load "eglot"
  (add-to-list 'eglot-server-programs
			   '(rust-mode "rust-analyzer")))

(provide 'init-lsp)
