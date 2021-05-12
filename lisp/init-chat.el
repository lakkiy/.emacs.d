;; -*- lexical-binding: t; -*-

(straight-use-package '(telega :type git :host github :branch "releases"))
(straight-use-package 'olivetti)

;; olivetti
(setq
 olivetti-body-width 80)

(autoload 'olivetti-mode "olivetti" nil t)

;; telega
(autoload #'telega "telega" nil t)

(with-eval-after-load "telega"
  (add-hook 'telega-root-mode-hook 'olivetti-mode)
  (add-hook 'telega-chat-mode-hook 'olivetti-mode))

(provide 'init-chat)
