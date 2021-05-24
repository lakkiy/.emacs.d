;;; -*- lexical-binding: t -*-

(straight-use-package 'flycheck)

;;; flycheck
(add-hook 'go-mode-hook 'flycheck-mode)

(with-eval-after-load "flycheck"
  (define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error)
  (setq flycheck-idle-change-delay 1))

(provide 'init-flycheck)
