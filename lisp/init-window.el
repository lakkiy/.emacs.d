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
   '(aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 2.0))))
   '(aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 1.0))))
   '(aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))))

(eat-package eyebrowse
  :straight t
  :hook (after-init-hook . eyebrowse-mode)
  :init
  (setq eyebrowse-new-workspace t)
  ;; (eyebrowse-setup-opinionated-keys) ;; FIXME conflict with `avy'
  (eyebrowse-mode t)
  :config
  (setq eyebrowse-mode-line-separator "|"))

(eat-package popper
  :straight t
  :init
  (setq popper-mode-line nil
        popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          "^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
          "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
          "^\\*term.*\\*$"   term-mode   ;term as a popup
          "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
          compilation-mode))
  (popper-mode +1)
  (when (not (display-graphic-p))
    (global-set-key (kbd "M-`") #'popper-toggle-latest)))

(provide 'init-window)
