;;; -*- lexical-binding: t -*-

(eat-package rg :straight t)

(eat-package yasnippet
  :straight t
  :commands yas-minor-mode
  :hook ((prog-mode-hook conf-mode-hook) . yas-minor-mode)
  :init
  (eat-package yasnippet-snippets :straight t)
  :config
  (let ((inhibit-message t))
    (yas-reload-all)))

(eat-package company
  :straight t
  :hook
  ((prog-mode-hook conf-mode-hook) . company-mode)
  :commands company-mode
  :init
  (setq company-vscode-icons-mapping nil
        company-begin-commands '(self-insert-command)
        company-idle-delay 0.2
        company-tooltip-limit 10
        company-tooltip-align-annotations t
        company-tooltip-width-grow-only t
        company-tooltip-idle-delay 0.4
        company-minimum-prefix-length 3
        company-dabbrev-downcase nil
        company-abort-manual-when-too-short t
        company-require-match nil
        company-global-modes '(not dired-mode dired-sidebar-mode)
        company-backends '((company-capf :with company-yasnippet)
                           (company-dabbrev-code company-keywords company-files)
                           company-dabbrev)
        company-tooltip-margin 0)
  :config
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-n") #'company-select-next))

(provide 'init-completion)
