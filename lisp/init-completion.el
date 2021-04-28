;;; -*- lexical-binding: t -*-

(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)

(autoload #'yas-minor-mode "yasnippet")

(add-hook 'prog-mode-hook 'yas-minor-mode)

(with-eval-after-load "yasnippet"
  (let ((inhibit-message t))
    (yas-reload-all)))


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


(straight-use-package 'deadgrep)

(autoload #'deadgrep "deadgrep" nil t)

(with-eval-after-load "deadgrep"
  (define-key deadgrep-mode-map (kbd "w") 'deadgrep-edit-mode)
  (define-key deadgrep-edit-mode-map (kbd "C-x C-s") 'deadgrep-mode))


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
         ("C-c r" . counsel-rg)
         ("C-c ." . counsel-unicode-char))
  :init
  (counsel-mode 1))

(provide 'init-completion)
