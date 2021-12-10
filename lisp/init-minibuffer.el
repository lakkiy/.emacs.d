;;; -*- lexical-binding: t -*-

(eat-package vertico
  :straight t
  :hook (after-init-hook . vertico-mode)
  :init
  (defun +minibuffer-backward-delete ()
    (interactive)
    (save-restriction
      (narrow-to-region (minibuffer-prompt-end) (point-max))
      (delete-region
       (save-mark-and-excursion
         (backward-sexp)
         (point))
       (point))))
  :config
  (define-key vertico-map (kbd "M-DEL") #'+minibuffer-backward-delete))

(eat-package orderless
  :straight t
  :after vertico
  :config
  (setq completion-styles '(substring orderless)))

(eat-package which-key
  :straight t
  :init
  ;; Allow C-h to trigger which-key before it is done automatically
  (setq which-key-show-early-on-C-h t)
  ;; make sure which-key doesn't show normally but refreshes quickly after it is
  ;; triggered.
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05))

(eat-package embark
  :straight (embark :files ("*.el"))
  :init
  (with-eval-after-load "vertico"
    (define-key vertico-map (kbd "M-o") 'embark-export)
    (define-key vertico-map (kbd "C-c C-o") 'embark-collect-snapshot)
    (define-key vertico-map (kbd "C-c C-c") 'embark-act))
  :config
  (define-key (kbd "<escape>") #'keyboard-escape-quit)
  ;; Consult users will also want the embark-consult package.
  (eat-package embark-consult :after consult)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

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

(provide 'init-minibuffer)
