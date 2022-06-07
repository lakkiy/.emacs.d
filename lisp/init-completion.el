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
  (eat/after-make-window-system-frame-hooks
   . (lambda ()
       (add-hook 'prog-mode-hook 'corfu-mode)
       (add-hook 'conf-mode-hook 'corfu-mode)
       (add-hook 'eshell-mode-hook 'corfu-mode)))
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

;; corfu-popup need this
(eat-package popon
  :straight (popon :type git :repo "https://codeberg.org/akib/emacs-popon.git"))

(eat-package corfu-terminal
  :straight (corfu-terminal :type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :hook (after-make-console-frame-hook . (lambda ()
                                           (add-hook 'prog-mode-hook 'eat/corfu-terminal-mode)
                                           (add-hook 'conf-mode-hook 'eat/corfu-terminal-mode)
                                           (add-hook 'eshell-mode-hook 'eat/corfu-terminal-mode)))
  :init
  (defun eat/corfu-terminal-mode ()
    (interactive)
    (corfu-terminal-mode +1)))

(eat-package corfu-doc
  :straight t
  :hook (corfu-mode-hook . corfu-doc-mode)
  :config
  (setq corfu-echo-documentation nil)
  (define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down)
  (define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up))

(eat-package copilot
  :straight (copilot :host github :repo "zerolfx/copilot.el"
                     :files ("dist" "copilot.el"))
  :hook (corfu-mode-hook . copilot-mode)
  :init
  (setq copilot-idle-delay eat/complete-delay)
  :config
  (global-set-key (kbd "C-<tab>") 'copilot-accept-completion)
  (with-eval-after-load 'meow
    (setq copilot-enable-predicates '(meow-insert-mode-p buffer-modified-p))))

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
  :hook (on-first-input-hook . marginalia-mode))

(eat-package isearch
  :init
  (setq
   ;; Match count next to the minibuffer prompt
   isearch-lazy-count t
   ;; Don't be stingy with history; default is to keep just 16 entries
   search-ring-max 200
   regexp-search-ring-max 200
   ;; htighlighted all matching
   isearch-lazy-highlight t
   lazy-highlight-buffer t
   ;; show search count, TODO not work in isearch-mb-mode
   lazy-count-prefix-format nil
   lazy-count-suffix-format " [%s/%s]"
   ;; Record isearch in minibuffer history, so C-x ESC ESC can repeat it.
   isearch-resume-in-command-history t
   ;; M-< and M-> move to the first/last occurrence of the current search string.
   isearch-allow-motion t
   isearch-motion-changes-direction t
   ;; space matches any sequence of characters in a line.
   isearch-regexp-lax-whitespace t
   search-whitespace-regexp ".*?")
  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "C-r") 'isearch-backward-regexp)
  :config
  (define-advice isearch-occur (:after (_regexp &optional _nlines))
    (isearch-exit))
  (define-key isearch-mode-map (kbd "C-c C-o") #'isearch-occur)
  (define-key isearch-mode-map [escape] #'isearch-cancel)
  ;; Edit the search string instead of jumping back
  (define-key isearch-mode-map [remap isearch-delete-chac] #'isearch-del-chac))

;; also chekc https://github.com/astoff/isearch-mb/wiki
(eat-package isearch-mb
  :straight t
  :hook (on-first-input-hook . isearch-mb-mode)
  :config
  (define-advice isearch-mb--update-prompt (:around (fn &rest _) show-case-fold-info)
    "Show case fold info in the prompt."
    (cl-letf* ((isearch--describe-regexp-mode-orig
                (symbol-function 'isearch--describe-regexp-mode))
               ((symbol-function 'isearch--describe-regexp-mode)
                (lambda (regexp-function &optional space-before)
                  (concat (if isearch-case-fold-search "[Case Fold] " "")
                          (funcall isearch--describe-regexp-mode-orig
                                   regexp-function space-before)))))
      (funcall fn _))))

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
  (global-set-key (kbd "C-c C-s") 'consult-line)
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
