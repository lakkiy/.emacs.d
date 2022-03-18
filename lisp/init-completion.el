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
  ((prog-mode-hook conf-mode-hook eshell-mode-hook) . company-mode)
  :commands company-mode
  :init
  (setq
   company-minimum-prefix-length 3
   company-idle-delay 0.2
   company-begin-commands '(self-insert-command
                            backward-delete-char)
   ;; icons
   company-vscode-icons-mapping nil
   company-text-icons-add-background t
   ;; thanks to r/emacs yyoncho
   company-format-margin-function 'company-text-icons-margin
   ;; tooltip frontend config
   company-tooltip-align-annotations t
   company-tooltip-limit 10
   company-tooltip-width-grow-only t
   company-tooltip-idle-delay 0.4
   company-dabbrev-downcase nil
   company-abort-manual-when-too-short t
   company-require-match nil
   company-global-modes '(not dired-mode dired-sidebar-mode)
   company-backends '((company-capf :with company-yasnippet)
                      (company-dabbrev-code company-keywords company-files)
                      company-dabbrev)
   company-files-exclusions '(".git/" ".DS_Store")
   company-tooltip-margin 0)
  :config
  (eat-package company-quickhelp
    :straight t
    :init
    (company-quickhelp-mode))
  (defun +complete ()
    (interactive)
    (or (yas/expand)
        (company-complete-selection)))
  (define-key company-active-map [tab] '+complete)
  (define-key company-active-map (kbd "TAB") '+complete)
  (define-key company-active-map [return] nil)
  (define-key company-active-map (kbd "RET") nil))

(eat-package vertico
  :straight (vertico :files (:defaults "extensions/*"))
  :hook (after-init-hook . vertico-mode)
  :config
  (define-key vertico-map (kbd "C-j") #'(lambda () (interactive)
                                          (if minibuffer--require-match
                                              (minibuffer-complete-and-exit)
                                            (exit-minibuffer))))
  (eat-package vertico-directory
    :hook (rfn-eshadow-update-overlay-hook . vertico-directory-tidy)
    :init
    (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
    (define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)
    (define-key vertico-map (kbd "RET") #'vertico-directory-enter))
  ;; (eat-package vertico-grid
  ;;   :init
  ;;   (setq vertico-grid-separator "    "))
  ;; (eat-package vertico-multiform
  ;;   :init
  ;;   (setq vertico-multiform-categories
  ;;         '((file grid reverse)
  ;;           (consult-location reverse)
  ;;           (consult-grep buffer)
  ;;           (minor-mode reverse)
  ;;           (imenu buffer)
  ;;           (t unobtrusive)))
  ;;   (vertico-multiform-mode)
  ;;   (define-key vertico-map (kbd "`") #'(lambda () (interactive)
  ;;                                             (vertico-multiform-unobtrusive)
  ;;                                             (vertico-multiform-reverse))))
  )

(eat-package orderless
  :straight t
  :after vertico
  :hook (minibuffer-setup-hook . sanityinc/use-orderless-in-minibuffer)
  :init
  (defun sanityinc/use-orderless-in-minibuffer ()
    (setq-local completion-styles '(substring orderless))))

(eat-package affe
  :straight t
  :init
  (defun sanityinc/affe-grep-at-point (&optional dir initial)
    (interactive (list prefix-arg (when-let ((s (symbol-at-point)))
                                    (symbol-name s))))
    (affe-grep dir initial)))

(eat-package marginalia
  :straight t
  :hook (after-init-hook . marginalia-mode))

(eat-package consult
  :straight t
  :init
  ;; In buffer action
  (global-set-key [remap isearch-forward] 'consult-line)
  (global-set-key [remap imenu] 'consult-imenu)
  (global-set-key [remap goto-line] 'consult-goto-line)
  (global-set-key [remap yank-pop] 'consult-yank-pop)
  (global-set-key (kbd "M-g o") 'consult-outline)
  ;; Disable preview
  (global-set-key [remap project-search] 'consult-ripgrep)
  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (global-set-key [remap bookmark-jump] 'consult-bookmark)
  (global-set-key [remap recentf-open-files] 'consult-recent-file)
  (setq consult-project-root-function (lambda ()
                                        (when-let (project (project-current))
                                          (car (project-roots project)))))
  :config
  (with-no-warnings
    (consult-customize consult-ripgrep consult-git-grep consult-grep
                       consult-bookmark
                       consult-recent-file
                       consult-buffer
                       :preview-key nil)))

(eat-package consult-yasnippet :straight t)

(eat-package embark
  :straight (embark :files ("*.el"))
  :init
  (with-eval-after-load "vertico"
    (define-key vertico-map (kbd "C-c C-o") 'embark-export)
    (define-key vertico-map (kbd "C-c C-c") 'embark-act))
  :config
  (define-key embark-meta-map (kbd "<escape>") #'keyboard-escape-quit)
  ;; Consult users will also want the embark-consult package.
  (eat-package embark-consult
    :after consult
    :hook (embark-collect-mode-hook . embark-consult-preview-minor-mode))
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(provide 'init-completion)
