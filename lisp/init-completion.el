;;; -*- lexical-binding: t -*-

(eat-package rg :straight t)

(eat-package yasnippet
  :straight t
  :commands yas-minor-mode
  :hook ((prog-mode-hook conf-mode-hook) . yas-minor-mode)
  :config
  (let ((inhibit-message t))
    (yas-reload-all)))

(eat-package company
  :straight t
  :hook
  ((prog-mode-hook conf-mode-hook) . company-mode)
  ;; Add `company-elisp' backend for elisp.
  (emacs-lisp-mode-hook . (lambda ()
                            (add-to-list 'company-backends #'company-elisp)))
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

(eat-package company-english-helper
  :straight
  (company-english-helper :type git :host github :repo "manateelazycat/company-english-helper")
  :commands toggle-company-english-helper
  :init
  (global-set-key (kbd "C-c I") #'toggle-company-english-helper))

(eat-package company-tabnine
  :straight t
  :doc "run company-tabnine-install-binary at the first time"
  ;; :after company-mode go-mode
  :config
  (add-to-list 'company-backends #'company-tabnine)
  (defun +remove-company-tabnine ()
    "Remove company-tabnine from company-backends"
    (interactive)
    (setq-default company-backends (remove 'company-tabnine company-backends))))

(eat-package bibtex-actions
  :straight t
  :after embark
  :config
  ;; Make the 'bibtex-actions' bindings available from `embark-act'.
  (add-to-list 'embark-keymap-alist '(bibtex . bibtex-actions-map))
  (setq bibtex-completion-bibliography "~/Dropbox/bib/references.bib"))

(provide 'init-completion)
