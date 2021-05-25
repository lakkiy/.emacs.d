;;; -*- lexical-binding: t -*-
(defun meow-setup-header-indicator ()
  (unless (-contains? header-line-format '(:eval (meow-indicator)))
    (setq-default header-line-format (append '((:eval (meow-indicator))) header-line-format))))

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
    "")
  (meow-setup-header-indicator)

  (custom-set-faces
   `(meow-keypad-indicator ((t (:inherit nano-face-header-popout))))
   `(meow-insert-indicator ((t (:inherit nano-face-header-critical))))
   `(meow-normal-indicator ((t (:inherit nano-face-header-faded))))
   `(meow-motion-indicator ((t (:inherit nano-face-header-popout)))))

  ;; TODO FIXME
  ;; cursot color and line number
  ;; and font on macos
  (when (eq system-type 'darwin)
    (tool-bar-mode -1)))

(provide 'init-nano)
