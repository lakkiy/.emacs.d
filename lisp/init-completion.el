;;; -*- lexical-binding: t -*-

(leaf yasnippet
  :straight t
  :hook
  (prog-mode-hook . yas-minor-mode)
  :config
  (let ((inhibit-message t)) (yas-reload-all)))

(leaf yasnippet-snippets
  :straight t)

(leaf company
  :straight t
  :bind (("M-/" . company-complete)
         ("C-M-i" . company-complete)
         (:company-mode-map
          ("<backtab>" . company-yasnippet))
         (:company-active-map
          ("C-p" . company-select-previous)
          ("C-n" . company-select-next)
          ("<tab>" . company-complete-common-or-cycle))
         (:company-search-map
          ("C-p" . company-select-previous)
          ("C-n" . company-select-next)))
  :hook (after-init-hook . global-company-mode)
  :setq
  ;; company-capf ignore case completion
  ;; (completion-ignore-case . t)
  :custom
  (company-idle-delay . 0.2)
  (company-tooltip-idle-delay . 0.1)
  (company-tooltip-limit . 10)
  (company-tooltip-align-annotations . t)
  (company-tooltip-width-grow-only . t)
  (company-dabbrev-downcase . nil)
  (company-global-modes . '(not org-mode dired-mode dired-sidebar-mode))
  (company-require-match . nil))

(leaf deadgrep
  :doc "use ripgrep from Emacs"
  :straight t
  :bind ((:deadgrep-mode-map
          ("w" . deadgrep-edit-mode))
         (:deadgrep-edit-mode-map
          ("C-x C-s" . deadgrep-mode)))
  :commands
  (deadgrep))

(leaf ivy
  :straight t
  :pre-setq
  (ivy-initial-inputs-alist . nil)
  :custom
  (ivy-count-format . "%d/%d ")
  (ivy-use-selectable-prompt . t)
  :init
  (ivy-mode 1))

(leaf counsel
  :straight t
  :bind (("C-s" . swiper)
         ("C-c z" . counsel-fzf)
         ("C-c r" . counsel-rg))
  :init
  (counsel-mode 1))

(leaf ivy-xref
  :straight
  (ivy-xref :type git :host github :repo "alexmurray/ivy-xref")
  :pre-setq
  (xref-show-definitions-function . #'ivy-xref-show-defs)
  (xref-show-xrefs-function . #'ivy-xref-show-xrefs))

;; Jump to definition, used as a fallback of lsp-find-definition
(leaf dumb-jump
  :straight t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate t)
  :bind (("M-g j" . dumb-jump-go)
         ("M-g J" . dumb-jump-go-other-window))
  :custom
  (dumb-jump-quiet . t)
  (dumb-jump-aggressive . t)
  (dumb-jump-selector . 'ivy)
  (dumb-jump-prefer-searcher . 'rg)
  (dumb-jump-disable-obsolete-warnings . t))


(provide 'init-completion)
