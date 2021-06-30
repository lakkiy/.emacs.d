;;; -*- lexical-binding: t -*-

(eat-package citre
  ;; :straight t
  :init
  ;; FIXME
  (straight-use-package '(citre :type git :host github :repo "universal-ctags/citre"))
  (global-set-key (kbd "C-x c p") 'citre-peek)
  :config
  (global-set-key (kbd "C-x c j") 'citre-jump)
  (global-set-key (kbd "C-x c J") 'citre-jump-back)
  (global-set-key (kbd "C-x c P") 'citre-ace-peek)
  (with-eval-after-load 'c-mode
    (require 'citre-lang-c)
    (add-hook 'c-mode-hook #'citre-auto-enable-citre-mode))
  (with-eval-after-load 'dired (require 'citre-lang-fileref)))

(eat-package pluse
  :init
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
  :hook
  ((dump-jump-after-jump-hook bookmark-after-jump-hook imenu-after-jump-hook) . recenter-and-pulse))

(eat-package devdocs
  :init
  (straight-use-package '(devdocs :type git :host github :repo "astoff/devdocs.el"))
  (global-set-key (kbd "C-c b") 'devdocs-lookup))

(eat-package docstr
  :straight t
;; FIXME not work with meow in go mode
  :hook (prog-mode-hook . (lambda () (docstr-mode 1))))

(eat-package flymake-mode
  :commands flymake-mode
  :config
  (define-key flymake-mode-map (kbd "C-c C-b") 'flymake-show-diagnostics-buffer)
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

(eat-package eglot
  :straight t
  :commands
  eglot-ensure
  eglot
  :hook (go-mode-hook . eglot-ensure)
  :init
  (setq eglot-stay-out-of nil
        eglot-ignored-server-capabilites '(:documentHighlightProvider))
  ;; auto expand function param for golang
  (setq-default eglot-workspace-configuration
                '((gopls
                   (usePlaceholders . t))))
  :config
  (add-to-list 'eglot-server-programs
			   '(rust-mode "rust-analyzer")))

(require 'init-go)
(require 'init-python)

(provide 'init-dev)
