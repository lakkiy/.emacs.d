;; -*- lexical-binding: t; -*-

(straight-use-package 'docker)
(straight-use-package 'docker-compose-mode)
(straight-use-package 'dockerfile-mode)

;; docker
(autoload 'docker "docker" nil t)

;; TODO protobuf mode

;;; protobuf-mode
(with-eval-after-load "protobuf-mode"
  (add-hook 'protobuf-mode-hook (lambda ()
                                  (setq imenu-generic-expression
                                        '((nil "^[[:space:]]*\\(message\\|service\\|enum\\)[[:space:]]+\\([[:alnum:]]+\\)" 2))))))

(provide 'init-spcfile)
