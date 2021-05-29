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
(straight-use-package 'selectrum)

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

;;; selectrum
(add-hook 'after-init-hook 'selectrum-mode)

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

(with-eval-after-load "selectrum"
  (define-key selectrum-minibuffer-map (kbd "M-DEL") #'+minibuffer-backward-delete)
  (define-key selectrum-minibuffer-map (kbd "{") #'selectrum-previous-candidate)
  (define-key selectrum-minibuffer-map (kbd "}") #'selectrum-next-candidate)
  (define-key selectrum-minibuffer-map (kbd "[") #'previous-history-element)
  (define-key selectrum-minibuffer-map (kbd "]") #'next-history-element))

;;; orderless
(setq
 selectrum-highlight-candidates-function #'orderless-highlight-matches
 selectrum-refine-candidates-function #'orderless-filter
 completion-styles '(substring orderless))

(with-eval-after-load "selectrum"
  (require 'orderless))

;;; consult
(setq
 xref-show-xrefs-function #'consult-xref
 xref-show-definitions-function #'consult-xref
 consult-project-root-function #'vc-root-dir)

(global-set-key (kbd "C-s") 'consult-line)

(with-eval-after-load "consult"
  (dolist (cmd '(consult-ripgrep))
    (add-to-list 'consult-config
                 `(,cmd :preview-key ,(kbd "M-P")))))

;;; embark
(with-eval-after-load "selectrum"
  (define-key selectrum-minibuffer-map (kbd "C-c C-o") 'embark-export)
  (define-key selectrum-minibuffer-map (kbd "C-c C-c") 'embark-act)
  (define-key selectrum-minibuffer-map (kbd "C-h B") 'embark-bindings))

;;; embark-consult
(with-eval-after-load 'embark
  (with-eval-after-load "consult"
    (require 'embark-consult)
    (add-hook 'embark-collect-mode-hook 'embark-consult-preview-minor-mode)))

;;; marginalia
(add-hook 'after-init-hook 'marginalia-mode)

(provide 'init-completion)
