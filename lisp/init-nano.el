;;; -*- lexical-binding: t -*-

(defun my/load-nano ()
  (interactive)
  (if (eq footheme 'nano-light)
      (require 'nano-theme-light))
  (if (eq footheme 'nano-dark)
      (require 'nano-theme-dark))
  (when (called-interactively-p 'any)
    (require 'nano-theme-light))
  (require 'nano-faces)
  (nano-faces)
  (require 'nano-theme)
  (nano-theme)

  (require 'nano-modeline)
  (defun nano-modeline-status ()
    "Overriding nano func. Retuen meow indicate"
    (meow-indicator))

  (when (eq system-type 'darwin)
    (tool-bar-mode -1)))

(provide 'init-nano)
