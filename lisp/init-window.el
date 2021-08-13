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

(eat-package eyebrowse
  :straight t
  :hook (after-init-hook . eyebrowse-mode)
  :init
  (setq eyebrowse-new-workspace t)
  :config
  (setq eyebrowse-mode-line-separator "|"))

;; Undo/redo changes to Emacs' window layout
(eat-package winner
  :hook (after-init-hook . winner-mode)
  :config
  (setq winner-dont-bind-my-keys t))

(provide 'init-window)
