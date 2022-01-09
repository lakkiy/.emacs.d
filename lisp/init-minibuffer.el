;;; -*- lexical-binding: t -*-

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
  (eat-package vertico-grid
    :init
    (setq vertico-grid-separator "    "))
  (eat-package vertico-multiform
    :init
    (setq vertico-multiform-categories
          '((file grid reverse)
            (consult-location reverse)
            (consult-grep buffer)
            (minor-mode reverse)
            (imenu buffer)
            (t unobtrusive)))
    (vertico-multiform-mode)
    ;; TODO can't toggle back
    (define-key vertico-map (kbd "M-TAB") #'(lambda () (interactive)
                                                  (vertico-multiform-unobtrusive)
                                                  (vertico-multiform-reverse)))))

(eat-package orderless
  :straight t
  :after vertico
  :config
  ;; do not use `orderless' style in company capf
  (define-advice company-capf
      (:around (orig-fun &rest args) set-completion-styles)
    (let ((completion-styles '(basic partial-completion)))
      (apply orig-fun args)))
  ;; set this local in minibuffer will break perl style split of
  ;; consult async commands like `consult-ripgrep'
  (setq completion-styles '(basic orderless)))

(eat-package which-key
  :straight t
  :init
  ;; Allow C-h to trigger which-key before it is done automatically
  (setq which-key-show-early-on-C-h t)
  ;; make sure which-key doesn't show normally but refreshes quickly after it is
  ;; triggered.
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05))

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

(eat-package embark
  :straight (embark :files ("*.el"))
  :init
  (with-eval-after-load "vertico"
    (define-key vertico-map (kbd "M-o") 'embark-export)
    (define-key vertico-map (kbd "C-c C-o") 'embark-collect-snapshot)
    (define-key vertico-map (kbd "C-c C-c") 'embark-act))
  :config
  (define-key embark-meta-map (kbd "<escape>") #'keyboard-escape-quit)
  ;; Consult users will also want the embark-consult package.
  (eat-package embark-consult :after consult)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(provide 'init-minibuffer)
