;;; -*- lexical-binding: t -*-

(eat-package ace-window
  :straight t
  :commands
  ace-swap-window
  ace-window
  :init
  (setq aw-keys '(?a ?o ?e ?u ?i)
        aw-scope 'frame)
  (custom-set-faces
   '(aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 2.0))))))

;; TODO show `eyebrowse-mode-line-indicator'
(eat-package eyebrowse
  :straight t
  :hook (after-init-hook . eyebrowse-mode)
  :init
  (setq eyebrowse-new-workspace t)
  :config
  (setq eyebrowse-mode-line-separator "|"))

(provide 'init-window)
