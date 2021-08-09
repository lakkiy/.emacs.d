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

(eat-package vertico
  :straight t
  :hook (after-init-hook . vertico-mode)
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
  (define-key vertico-map (kbd "M-DEL") #'+minibuffer-backward-delete))

(eat-package orderless
  :straight t
  :after vertico
  :config
  (setq completion-styles '(substring orderless)))

(eat-package consult
  :straight t
  :init
  (global-set-key (kbd "C-s") 'consult-line)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
        consult-project-root-function (lambda ()
                                        (when-let (project (project-current))
                                          (car (project-roots project))))))

(eat-package embark
  :straight t
  :after vertico
  :init
  :config
  (eat-package embark-consult :after consult)
  (define-key vertico-map (kbd "C-c C-o") 'embark-export)
  (define-key vertico-map (kbd "C-c C-c") 'embark-act)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(eat-package marginalia
  :straight t
  :hook (after-init-hook . marginalia-mode))

(eat-package bibtex-actions
  :straight t
  :after embark
  :config
  ;; Make the 'bibtex-actions' bindings available from `embark-act'.
  (add-to-list 'embark-keymap-alist '(bibtex . bibtex-actions-map))
  (setq bibtex-completion-bibliography "~/Dropbox/bib/references.bib"))

(provide 'init-completion)
