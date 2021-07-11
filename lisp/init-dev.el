;;; -*- lexical-binding: t -*-

(eat-package tree-sitter
  :straight t
  :init
  (eat-package tree-sitter-langs :straight t)
  :hook
  ((go-mode-hook
    python-mode-hook
    agda-mode-hook
    c-mode-hook
    c++-mode-hook
    css-mode-hook
    html-mode-hook
    js-mode-hook
    js2-mode-hook
    son-mode-hook
    ruby-mode-hook
    rust-mode-hook
    typescript-mode-hook)
   . tree-sitter-mode)
  (tree-sitter-after-on-hook . tree-sitter-hl-mode)
  :config
  (require 'tree-sitter-langs))

(eat-package citre
  :straight (citre :type git :host github :repo "universal-ctags/citre")
  :commands citre-update-tags-file
  :init
  (setq citre-default-create-tags-file-location 'global-cache
        citre-use-project-root-when-creating-tags t
        citre-prompt-language-for-ctags-command t)
  (global-set-key (kbd "C-x c p") 'citre-peek)
  :config
  (global-set-key (kbd "C-x c j") 'citre-jump)
  (global-set-key (kbd "C-x c J") 'citre-jump-back)
  (global-set-key (kbd "C-x c P") 'citre-ace-peek)
  (with-eval-after-load 'c-mode
    (require 'citre-lang-c)
    (add-hook 'c-mode-hook #'citre-auto-enable-citre-mode))
  (with-eval-after-load 'dired (require 'citre-lang-fileref)))

(eat-package devdocs
  :straight (devdocs :type git :host github :repo "astoff/devdocs.el")
  :init
  (global-set-key (kbd "C-c b") 'devdocs-lookup))

(eat-package docstr
  :straight t
;; FIXME not work with meow in go mode
  :hook (prog-mode-hook . (lambda () (docstr-mode 1))))

(eat-package flymake
  :commands flymake-mode
  :after prog-mode
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
(require 'init-c)
(require 'init-web)

(provide 'init-dev)
