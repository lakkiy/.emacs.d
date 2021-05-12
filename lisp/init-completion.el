;;; -*- lexical-binding: t -*-

(straight-use-package 'consult)
(straight-use-package 'yasnippet)
(straight-use-package 'deadgrep)
(straight-use-package 'selectrum)
(straight-use-package 'orderless)
(straight-use-package 'company)
(straight-use-package 'marginalia)
(straight-use-package 'embark)

;; yasnippet
(autoload #'yas-minor-mode "yasnippet")

(add-hook 'prog-mode-hook 'yas-minor-mode)

(with-eval-after-load "yasnippet"
  (let ((inhibit-message t))
    (yas-reload-all)))

;; company
(setq
 company-vscode-icons-mapping nil
 company-begin-commands '(self-insert-command)
 company-idle-delay 0
 company-tooltip-limit 10
 company-tooltip-align-annotations t
 company-tooltip-width-grow-only t
 company-tooltip-idle-delay 0.4
 company-minimum-prefix-length 5
 company-dabbrev-downcase nil
 company-abort-manual-when-too-short t
 company-require-match nil
 company-global-modes '(not dired-mode dired-sidebar-mode)
 company-tooltip-margin 0)

(autoload #'company-mode "company")

(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'conf-mode-hook 'company-mode)
(add-hook 'eshell-mode-hook 'company-mode)

(with-eval-after-load "company"
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-n") #'company-select-next))

;; deadgrep
(autoload #'deadgrep "deadgrep" nil t)

(with-eval-after-load "deadgrep"
  (define-key deadgrep-mode-map (kbd "w") 'deadgrep-edit-mode)
  (define-key deadgrep-edit-mode-map (kbd "C-x C-s") 'deadgrep-mode))

;; selectrum
(require 'selectrum)
(selectrum-mode t)

;; consult
(require 'consult)

(setq consult-project-root-function #'vc-root-dir)

(with-eval-after-load "consult"
  (global-set-key (kbd "C-s") 'consult-line))

;; orderless
(require 'orderless)
(setq completion-styles '(orderless))
(setq selectrum-refine-candidates-function #'orderless-filter)
(setq selectrum-highlight-candidates-function #'orderless-highlight-matches)

;; marginalia
(marginalia-mode)

;; embark
(setq prefix-help-command #'embark-prefix-help-command)

(with-eval-after-load 'embark
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (global-set-key (kbd "C-S-a") 'embark-act)
  (global-set-key (kbd "C-h B") 'embark-bindings)
  (with-eval-after-load 'consult
    (add-hook 'embark-collect-mode-hook 'embark-consult-preview-minor-mode)))

(provide 'init-completion)
