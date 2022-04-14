;; don't need this

(eat-package display-fill-column-indicator
  :hook (prog-mode-hook . display-fill-column-indicator-mode)
  :init
  (setq-default indicate-buffer-boundaries 'left)
  (setq-default display-fill-column-indicator-character ?\u254e))
