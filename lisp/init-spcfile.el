;; -*- lexical-binding: t; -*-

(leaf docker
  :straight t
  :commands docker)

(leaf docker-compose-mode :straight t)
(leaf dockerfile-mode
  :straight t
  :mode "Dockerfile\\'")

(leaf protobuf-mode
  :straight
  (protobuf-mode :type git
                 :host github
                 :repo "protocolbuffers/protobuf")
  :mode "\\.proto\\'")

(provide 'init-spcfile)
