;; -*- lexical-binding: t; -*-

(eat-package docker
  :straight t
  :commands docker)

(eat-package docker-compose-mode :straight t)
(eat-package dockerfile-mode :straight t)

(eat-package markdown-mode
  :straight t
  :mode ("\\.md\\" . gfm-mode)
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

(eat-package fish-mode
  :straight t
  :config
  (add-hook 'fish-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        #'fish_indent-before-save))))

;; TODO download single file
;;; protobuf-mode
(with-eval-after-load "protobuf-mode"
  (add-hook 'protobuf-mode-hook
            (lambda ()
              (setq imenu-generic-expression
                    '((nil "^[[:space:]]*\\(message\\|service\\|enum\\)[[:space:]]+\\([[:alnum:]]+\\)" 2))))))

(provide 'init-spcfile)
