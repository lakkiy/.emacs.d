;;; -*- lexical-binding: t -*-

(use-package yasnippet
  :config
  (let ((inhibit-message t)) (yas-reload-all))
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode))

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
  :custom
  (company-idle-delay . 0.2)
  (company-tooltip-idle-delay . 0.1)
  (company-tooltip-limit . 10)
  (company-tooltip-align-annotations . t)
  (company-tooltip-width-grow-only . t)
  (company-dabbrev-downcase . nil)
  (company-global-modes . '(not org-mode dired-mode dired-sidebar-mode))
  (company-require-match . nil))

;; (use-package company
;;   :bind (("M-/" . company-complete)
;;          ("C-M-i" . company-complete)
;;          :map company-mode-map
;;          ("<backtab>" . company-yasnippet)
;;          :map company-active-map
;;          ("C-p" . company-select-previous)
;;          ("C-n" . company-select-next)
;;          ("<tab>" . company-complete-common-or-cycle)
;;          :map company-search-map
;;          ("C-p" . company-select-previous)
;;          ("C-n" . company-select-next))
;;   :hook (after-init . global-company-mode)
;;   :custom
;;   (company-idle-delay 0.2)
;;   (company-tooltip-idle-delay 0.1)
;;   (company-tooltip-limit 10)
;;   (company-tooltip-align-annotations t)
;;   (company-tooltip-width-grow-only t)
;;   (company-dabbrev-downcase nil)
;;   (company-global-modes '(not org-mode dired-mode dired-sidebar-mode))
;;   (company-require-match nil))

(leaf ivy
  :straight t
  :pre-setq
  (ivy-initial-inputs-alist . nil)
  :custom
  (ivy-count-format . "%d/%d ")
  (ivy-use-selectable-prompt . t)
  :init
  (ivy-mode 1))

;; (use-package ivy
;;   :init
;;   (setq ivy-initial-inputs-alist nil)
;;   (ivy-mode 1)
;;   :custom
;;   (ivy-count-format "%d/%d ")
;;   (ivy-use-selectable-prompt t))

(leaf counsel
  :straight t
  :bind (("C-s" . swiper)
         ("C-c z" . counsel-fzf)
         ("C-c r" . counsel-rg))
  :init
  (counsel-mode 1))

;; (use-package counsel
;;   :bind
;;   ("C-s" . swiper)
;;   ("C-c z" . counsel-fzf)
;;   ("C-c r" . counsel-rg)
;;   :init
;;   (counsel-mode 1))

(leaf ivy-xref
  :straight
  (ivy-xref :type git :host github :repo "alexmurray/ivy-xref")
  :pre-setq
  (xref-show-definitions-function . #'ivy-xref-show-defs)
  (xref-show-xrefs-function . #'ivy-xref-show-xrefs))


(provide 'init-completion)
