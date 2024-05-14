;;; -*- lexical-binding: t -*-

;; orderless
(install-package 'orderless)

;; embark
(install-package 'embark)

(defun sanityinc/use-orderless-in-minibuffer ()
  (setq-local completion-styles '(substring orderless)))
(add-hook 'minibuffer-setup-hook #'sanityinc/use-orderless-in-minibuffer)

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


;; consult
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

;;; init-minibuffer.el ends here
(provide 'init-minibuffer)
