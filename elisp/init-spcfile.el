;; -*- lexical-binding: t; -*-

;; (use-package docker
;;   :commands (docker))
;;
;; (use-package docker-compose-mode)
;;
;; (use-package dockerfile-mode)

(leaf docker
  :straight t
  :commands docker)

(leaf docker-compose-mode :straight t)
(leaf dockerfile-mode :straight t)

(provide 'init-spcfile)
