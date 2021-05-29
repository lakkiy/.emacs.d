;;; -*- lexical-binding: t -*-

(straight-use-package 'consult)
(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)
(straight-use-package 'deadgrep)
(straight-use-package 'orderless)
(straight-use-package 'company)
(straight-use-package 'marginalia)
(straight-use-package 'embark)
(straight-use-package 'embark-consult)
(straight-use-package 'company-tabnine)
(straight-use-package '(vertico :type git :host github :repo "minad/vertico"))
(straight-use-package '(affe :type git :host github :repo "minad/affe"))

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
  ;;; company-tabnine run company-tabnine-install-binary at the first time
  (add-to-list 'company-backends #'company-tabnine)

  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-n") #'company-select-next))

;; deadgrep
(autoload #'deadgrep "deadgrep" nil t)

(with-eval-after-load "deadgrep"
  (define-key deadgrep-mode-map (kbd "w") 'deadgrep-edit-mode)
  (define-key deadgrep-edit-mode-map (kbd "C-x C-s") 'deadgrep-mode))

;;; vertico
(add-hook 'after-init-hook 'vertico-mode)
;; disable the automatic *Completions* buffer
(advice-add #'vertico--setup :after
            (lambda (&rest _)
              (setq-local completion-auto-help nil
                          completion-show-inline-help nil)))

;;; orderless
(setq
 completion-styles '(substring orderless))

(with-eval-after-load "vertico"
  (require 'orderless))

;;; consult
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref
      consult-project-root-function #'vc-root-dir)

(global-set-key (kbd "C-s") 'consult-line)

(with-eval-after-load "consult"
  (dolist (cmd '(consult-ripgrep affe-grep))
    (add-to-list 'consult-config
                 `(,cmd :preview-key ,(kbd "M-P")))))

;;; embark
(with-eval-after-load "vertico"
  (define-key vertico-map (kbd "C-c C-o") 'embark-export)
  (define-key vertico-map (kbd "C-c C-c") 'embark-act)
  (define-key vertico-map (kbd "C-h B") 'embark-bindings))

;;; embark-consult
(with-eval-after-load 'embark
  (with-eval-after-load "consult"
    (require 'embark-consult)
    (add-hook 'embark-collect-mode-hook 'embark-consult-preview-minor-mode)))

;;; marginalia
(add-hook 'after-init-hook 'marginalia-mode)

(provide 'init-completion)
