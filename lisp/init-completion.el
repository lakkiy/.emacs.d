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

(eat-package corfu
  :straight (corfu :files (:defaults "extensions/*.el"))
  :hook
  ;; TODO didn't enable corfu mode under tui
  ((prog-mode-hook conf-mode-hook eshell-mode-hook) . corfu-mode)
  :init
  (setq
   corfu-preview-current nil
   corfu-auto-delay eat/complete-delay
   corfu-auto-prefix 2
   corfu-quit-no-match t
   corfu-quit-at-boundary t
   corfu-auto t)
  :config
  (defun eat/corfu-complete ()
    (interactive)
    (or (yas-expand)
        ;; NOTE `corfu-complete' sometimes didn't quit corfu after complete
        (corfu-insert)))
  ;; quit corfu completion and back to meow normal mode when it enable
  (define-key corfu-map (kbd "<escape>") #'(lambda ()
                                             (interactive)
                                             (corfu-quit)
                                             (when (meow-insert-mode-p)
                                               (meow-insert-exit))))
  (define-key corfu-map (kbd "<tab>") 'eat/corfu-complete)
  (define-key corfu-map (kbd "TAB") 'eat/corfu-complete)
  (define-key corfu-map (kbd "RET") nil))

;; `corfu-terminal' need this
(eat-package popon
  :straight (popon :type git :repo "https://codeberg.org/akib/emacs-popon.git"))

(eat-package corfu-terminal
  :straight (corfu-terminal :type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :hook
  (corfu-mode-hook . (lambda ()
                       (unless (display-graphic-p)
                         (corfu-terminal-mode +1)))))

(eat-package corfu-doc
  :straight t
  :hook (corfu-mode-hook . corfu-doc-mode)
  :config
  (setq corfu-echo-documentation nil)
  (define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down)
  (define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up))

(eat-package vertico
  :straight (vertico :files (:defaults "extensions/*"))
  :init
  (setq eat/minibuffer-completion-function 'vertico-mode)
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
    (define-key vertico-map (kbd "RET") #'vertico-directory-enter)))

(eat-package orderless
  :straight t
  :after vertico
  :hook (minibuffer-setup-hook . sanityinc/use-orderless-in-minibuffer)
  :init
  (defun sanityinc/use-orderless-in-minibuffer ()
    (setq-local completion-styles '(substring orderless)))
  :config
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

(eat-package marginalia
  :straight t
  ;; FIXME hook is nil
  :hook (on-first-input-hook . marginalia-mode))

(eat-package embark
  :straight (embark :files ("*.el"))
  :init
  (with-eval-after-load "vertico"
    (define-key vertico-map (kbd "C-c C-o") 'embark-export)
    (define-key vertico-map (kbd "C-c C-c") 'embark-act))
  :config
  ;; Consult users will also want the embark-consult package.
  (eat-package embark-consult
    :after embark consult
    :hook (embark-collect-mode-hook . embark-consult-preview-minor-mode))
  (define-key embark-meta-map (kbd "<escape>") #'keyboard-escape-quit)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(eat-package consult
  :straight t
  :init
  (eat-package consult-yasnippet :straight t)
  ;; In buffer action
  (global-set-key (kbd "C-s") 'consult-line)
  (global-set-key [remap imenu] 'consult-imenu)
  (global-set-key [remap goto-line] 'consult-goto-line)
  (global-set-key [remap yank-pop] 'consult-yank-pop)
  (global-set-key (kbd "M-g o") 'consult-outline)
  ;; Disable preview
  (global-set-key [remap project-find-regexp] 'consult-ripgrep)
  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (global-set-key [remap bookmark-jump] 'consult-bookmark)
  (global-set-key [remap recentf-open-files] 'consult-recent-file)
  (setq consult-project-root-function (lambda ()
                                        (when-let (project (project-current))
                                          (car (project-roots project)))))
  :config
  ;; (global-set-key (kbd "C-c C-s") #'consult-line)
  (with-no-warnings
    (consult-customize consult-ripgrep consult-git-grep consult-grep
                       consult-bookmark
                       consult-recent-file
                       consult-buffer
                       :preview-key nil)))

;;; init-completion.el ends here
(provide 'init-completion)
