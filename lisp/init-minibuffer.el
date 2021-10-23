;;; -*- lexical-binding: t -*-

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

(eat-package vertico-posframe
  :after vertico
  :straight (vertico-posframe :type git :host github :repo "tumashu/vertico-posframe")
  :init
  (vertico-posframe-mode 1))

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
