;;; -*- lexical-binding: t -*-

(straight-use-package 'consult)
(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)
(straight-use-package 'deadgrep)
(straight-use-package 'selectrum)
(straight-use-package 'selectrum-prescient)
(straight-use-package 'marginalia)
(straight-use-package 'embark)


;; yasnippet
(autoload #'yas-minor-mode "yasnippet")

(add-hook 'prog-mode-hook 'yas-minor-mode)

(with-eval-after-load "yasnippet"
  (let ((inhibit-message t))
    (yas-reload-all)))

;; TODO company
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

;; deadgrep
(autoload #'deadgrep "deadgrep" nil t)

(with-eval-after-load "deadgrep"
  (define-key deadgrep-mode-map (kbd "w") 'deadgrep-edit-mode)
  (define-key deadgrep-edit-mode-map (kbd "C-x C-s") 'deadgrep-mode))

;; selectrum
(require 'selectrum)
(require 'selectrum-prescient)
(selectrum-mode t)
(selectrum-prescient-mode t)

;; consult
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

(require 'consult)

(with-eval-after-load "consult"
  (global-set-key (kbd "C-s") 'consult-line))

;; marginalia
(marginalia-mode)

(with-eval-after-load "marginalia"
  (define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle))

;; TODO embark

(provide 'init-completion)
