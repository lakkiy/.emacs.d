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
