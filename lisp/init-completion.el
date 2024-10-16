;;; -*- lexical-binding: t -*-

;; orderless
(install-package 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

;; embark
(install-package 'embark)

(keymap-global-set "C-." #'embark-act)
(keymap-set minibuffer-local-map "C-c C-o" #'embark-export)

(with-eval-after-load 'embark
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; vertico
(install-package 'vertico)

(add-hook 'after-init-hook #'vertico-mode)
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

(setq vertico-resize nil
      vertico-count 17)

(with-eval-after-load 'vertico
  (keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
  (keymap-set vertico-map "RET" #'vertico-directory-enter)
  (keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word))

;; marginalia
(install-package 'marginalia)
(add-hook 'vertico-mode-hook #'marginalia-mode)

;; icon
(install-package 'nerd-icons-completion)
(add-hook 'after-init-hook #'nerd-icons-completion-mode)
(add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)

;;; consult
(install-package 'consult)
(install-package 'embark-consult)
(install-package 'consult-dir)
(install-package 'consult-eglot)

(setq consult-narrow-key             "<"
      consult-preview-key            "M-."
      xref-show-xrefs-function       #'consult-xref
      xref-show-definitions-function #'consult-xref)

;; Use `consult-completion-in-region' if Vertico is enabled.
;; Otherwise use the default `completion--in-region' function.
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

(keymap-global-set "M-l"      #'consult-line)
(keymap-substitute global-map #'yank-pop #'consult-yank-pop)
;; C-x bindings
(keymap-substitute global-map #'switch-to-buffer #'consult-buffer)
(keymap-substitute global-map #'switch-to-buffer-other-window #'consult-buffer-other-window)
(keymap-substitute global-map #'switch-to-buffer-other-tab #'consult-buffer-other-tab)
(keymap-substitute global-map #'switch-to-buffer-other-frame #'consult-buffer-other-frame)
(keymap-substitute global-map #'project-switch-to-buffer #'consult-project-buffer)
(keymap-substitute global-map #'project-find-regexp #'consult-ripgrep)
(keymap-substitute global-map #'bookmark-jump #'consult-bookmark)
(keymap-substitute global-map #'recentf-open-files #'consult-recent-file)
;; M-g for go to things
(keymap-global-set "M-g e"    #'consult-compile-error)
(keymap-global-set "M-g f"    #'consult-flymake)
(keymap-global-set "M-g o"    #'consult-outline)
(keymap-global-set "M-g m"    #'consult-mark)
(keymap-substitute global-map #'imenu #'consult-imenu)
(keymap-substitute global-map #'goto-line #'consult-goto-line)
;; register
(keymap-global-set "M-#"   #'consult-register-load)
(keymap-global-set "C-M-'" #'consult-register-store)
(keymap-global-set "C-M-#" #'consult-register)

;; consult-dir
(keymap-substitute global-map #'list-directory #'consult-dir)
(with-eval-after-load 'vertico
  (keymap-set vertico-map "C-x C-d" #'consult-dir)
  (keymap-set vertico-map "C-x C-j" #'consult-dir-jump-file))

;; embark-consult
(with-eval-after-load 'consult
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

;; capf
(setq completion-ignore-case t)

;;; company
;;
;; - use C-p/C-n to select prev/next
;; - use tab to do complete
;; - free ret, popup will no longer interrupt typing
;; - when disable company, use =completion-at-point= to do it manually

(install-package 'company)

(add-hook 'after-init-hook 'global-company-mode)

;; Basic config.
(defun my-company-capf--candidates (func &rest args)
  "Try default completion styles."
  (let ((completion-styles '(basic partial-completion)))
    (apply func args)))
(advice-add 'company-capf--candidates :around 'my-company-capf--candidates)

(setq company-minimum-prefix-length 2
      company-require-match nil
      company-idle-delay 0.1)

;; Frontend
;;
;; don't need preview frontends with copilot
;; use C-h to show doc(maybe use eldoc-box) instead echo- frontend
;; so only config tooltip frontend
(setq company-frontends '(company-pseudo-tooltip-frontend)
      company-tooltip-align-annotations t
      ;; no matter if a tooltip is shown above or below point, the
      ;; candidates are always listed starting near point.
      company-tooltip-width-grow-only t
      ;; icon in front of a candidate, make it looks like Atom
      company-format-margin-function #'company-text-icons-margin
      company-text-icons-add-background t)

;; Backend
;;
;; Do not use company-capf with company-yasnippet, it will cause
;; completions list messed up.
(setq company-backends '(company-capf
                         company-files
                         (;;searching for completion candidates inside the contents of the open buffer(s)
                          company-dabbrev-code
                          ;; provides completions from programming language keywords
                          company-keywords)
                         company-dabbrev)
      company-files-exclusions '(".git/" ".DS_Store")
      ;; search from the buffers with the same majar mode
      company-dabbrev-other-buffers t
      company-dabbrev-ignore-case nil
      company-dabbrev-downcase nil
      company-dabbrev-code-ignore-case nil
      company-dabbrev-code-everywhere t)

;; Keybinding
(with-eval-after-load "company"
  (define-key company-active-map [tab] #'company-complete-selection)
  (define-key company-active-map (kbd "TAB") #'company-complete-selection)
  (define-key company-active-map (kbd "C-s") #'company-filter-candidates)
  ;; Free SPC and RET, popup will no longer interrupt typing.
  ;; use tab to do complete and free ret
  (define-key company-active-map [escape] nil)
  (define-key company-active-map [return] nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "SPC") nil))

;;; yasnippet
(install-package 'yasnippet)

(add-hook 'prog-mode-hook #'yas-minor-mode)
(add-hook 'conf-mode-hook #'yas-minor-mode)

(with-eval-after-load 'yasnippet
  (let ((inhibit-message t))
    (yas-reload-all)))

;;; init-completion.el ends here
