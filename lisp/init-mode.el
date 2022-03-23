;; -*- lexical-binding: t; -*-

;;  TODO csv crontab
;; TODO terraform
(eat-package toml-mode :straight t :hook (toml-mode-hook . goto-address-prog-mode))
(eat-package yaml-mode :straight t :hook (yaml-mode-hook . goto-address-prog-mode))
(eat-package docker-compose-mode :straight t)
(eat-package dockerfile-mode :straight t)
(eat-package k8s-mode :straight t)

(eat-package git-modes
  :straight t
  :init
  (add-to-list 'auto-mode-alist
               (cons "/.dockerignore\\'" 'gitignore-mode)))

(eat-package kubedoc
  :straight
  (kubedoc :type git :host github :repo "r0bobo/kubedoc.el"))

(eat-package markdown-mode
  :straight t
  :mode ("\\.md\\'" . gfm-mode)
  :hook (markdown-mode-hook . prose-mode)
  :init
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-uppercase-checkbox t
        markdown-fontify-code-blocks-natively t))

(eat-package markdown-toc
  :straight t
  :config
  (define-key markdown-mode-command-map (kbd "r") #'markdown-toc-generate-or-refresh-toc))

;; TODO download single file
;;; protobuf-mode
(with-eval-after-load "protobuf-mode"
  (add-hook 'protobuf-mode-hook
            (lambda ()
              (setq imenu-generic-expression
                    '((nil "^[[:space:]]*\\(message\\|service\\|enum\\)[[:space:]]+\\([[:alnum:]]+\\)" 2))))))

(provide 'init-mode)
