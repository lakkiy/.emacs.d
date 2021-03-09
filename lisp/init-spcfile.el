;; -*- lexical-binding: t; -*-

(leaf docker
  :straight t
  :commands docker)

(leaf docker-compose-mode :straight t)
(leaf dockerfile-mode
  :straight t
  :mode "Dockerfile\\'")

(provide 'init-spcfile)
