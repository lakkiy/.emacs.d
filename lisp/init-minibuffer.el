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

(eat-package consult
  :straight t
  :init
  (global-set-key (kbd "C-s") 'consult-line)
  (global-set-key [remap bookmark-jump] 'consult-bookmark)
  (global-set-key [remap recentf-open-files] 'consult-recent-file)
  (global-set-key [remap imenu] 'consult-imenu)
  (global-set-key [remap goto-line] 'consult-goto-line)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
        consult-project-root-function (lambda ()
                                        (when-let (project (project-current))
                                          (car (project-roots project))))))

(eat-package embark
  :straight t
  :init
  (with-eval-after-load "vertico"
    (define-key vertico-map (kbd "C-c C-o") 'embark-export)
    (define-key vertico-map (kbd "C-c C-c") 'embark-act))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(eat-package marginalia
  :straight t
  :hook (after-init-hook . marginalia-mode))

(provide 'init-minibuffer)
