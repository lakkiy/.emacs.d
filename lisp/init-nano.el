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

  ;; FIXME not work after override nano-modeline-status ??
  (custom-set-faces
   `(meow-keypad-indicator ((t (:inherit nano-face-header-popout))))
   `(meow-insert-indicator ((t (:inherit nano-face-header-critical))))
   `(meow-normal-indicator ((t (:inherit nano-face-header-faded))))
   `(meow-motion-indicator ((t (:inherit nano-face-header-popout))))))

(provide 'init-nano)
