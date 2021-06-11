;; -*- lexical-binding: t; -*-

(straight-use-package 'docker)
(straight-use-package 'docker-compose-mode)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'fish-mode)

(+pdump-packages 'docker
                 'docker-compose-mode
                 'dockerfile-mode
                 'fish-mode)

;;; fish-mode
(with-eval-after-load "fish-mode"
  (add-hook 'fish-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        #'fish_indent-before-save))))

;;; docker
(autoload 'docker "docker" nil t)

;;; protobuf-mode
(with-eval-after-load "protobuf-mode"
  (add-hook 'protobuf-mode-hook
            (lambda ()
              (setq imenu-generic-expression
                    '((nil "^[[:space:]]*\\(message\\|service\\|enum\\)[[:space:]]+\\([[:alnum:]]+\\)" 2))))))

(provide 'init-spcfile)
