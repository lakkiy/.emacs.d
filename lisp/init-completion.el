;;; -*- lexical-binding: t -*-

(eat-package rg :straight t)

(eat-package yasnippet
  :straight t
  :commands yas-minor-mode
  :hook (prog-mode-hook . yas-minor-mode)
  :config
  (let ((inhibit-message t))
    (yas-reload-all)))

(eat-package company
  :straight t
  :hook
  ((prog-mode-hook conf-mode-hook) . company-mode)
  ;; Add `company-elisp' backend for elisp.
  (emacs-lisp-mode-hook . (lambda ()
                            (require 'company-elisp)
                            (push 'company-elisp company-backends)))
  :commands company-mode
  :init
  (setq company-vscode-icons-mapping nil
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
        company-backends '((company-capf :with company-yasnippet)
                           (company-dabbrev-code company-keywords company-files)
                           company-dabbrev)
        company-tooltip-margin 0)
  :config
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-n") #'company-select-next))

(eat-package company-tabnine
  :straight t
  :doc "run company-tabnine-install-binary at the first time"
  :after company-mode go-mode
  :config
  (add-to-list 'company-backends #'company-tabnine))

(eat-package selectrum
  :straight t
  :hook
  (after-init-hook . (lambda ()
                       (require 'selectrum)
                       (selectrum-mode)))
  :init
  (defun +minibuffer-backward-delete ()
    (interactive)
    (delete-region
     (or
      (save-mark-and-excursion
        (while (equal ?/ (char-before)) (backward-char))
        (when-let ((p (re-search-backward "/" (line-beginning-position) t)))
          (1+ p)))
      (save-mark-and-excursion (backward-word) (point)))
     (point)))
  :config
  (define-key selectrum-minibuffer-map (kbd "M-DEL") #'+minibuffer-backward-delete)
  (define-key selectrum-minibuffer-map (kbd "{") #'selectrum-previous-candidate)
  (define-key selectrum-minibuffer-map (kbd "}") #'selectrum-next-candidate)
  (define-key selectrum-minibuffer-map (kbd "[") #'previous-history-element)
  (define-key selectrum-minibuffer-map (kbd "]") #'next-history-element))

(eat-package orderless
  :straight t
  :after selectrum
  :init
  (setq selectrum-highlight-candidates-function #'orderless-highlight-matches
        selectrum-refine-candidates-function #'orderless-filter
        completion-styles '(substring orderless)))

(eat-package consult
  :straight t
  :init
  (global-set-key (kbd "C-s") 'consult-line)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
        consult-project-root-function #'vc-root-dir))

(eat-package embark
  :straight t
  :after selectrum
  :config
  (define-key selectrum-minibuffer-map (kbd "C-c C-o") 'embark-export)
  ;; C-h show embark-act key bindings
  (define-key selectrum-minibuffer-map (kbd "C-c C-c") 'embark-act)
  (define-key selectrum-minibuffer-map (kbd "C-h B") 'embark-bindings))

(eat-package embark-consult
  :straight t
  :after embark consult
  :config
  (add-hook 'embark-collect-mode-hook 'embark-consult-preview-minor-mode))

(eat-package marginalia
  :straight t
  :hook (after-init-hook . marginalia-mode))

(provide 'init-completion)
