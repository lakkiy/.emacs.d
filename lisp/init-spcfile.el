;; -*- lexical-binding: t; -*-

(eat-package docker
  :straight t
  :commands docker)

(eat-package docker-compose-mode :straight t)
(eat-package dockerfile-mode :straight t)
(eat-package markdown-mode :straight t)

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
