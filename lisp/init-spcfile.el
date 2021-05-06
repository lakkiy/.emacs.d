;; -*- lexical-binding: t; -*-

(straight-use-package 'docker)
(straight-use-package 'docker-compose-mode)
(straight-use-package 'dockerfile-mode)

;; docker
(autoload 'docker "docker" nil t)

;; TODO protobuf mode

(provide 'init-spcfile)
