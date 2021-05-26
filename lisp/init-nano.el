;;; -*- lexical-binding: t -*-

(defun redefine-nano-face-font-lock ()
  "Override some nano face or it will chnage some font family to Roboto Mono"
  (set-face-attribute 'nano-face-strong nil
                      :foreground (face-foreground 'nano-face-default)
                      :weight 'medium)
  (set-face-attribute 'nano-face-tag-default nil
                      :foreground nano-color-foreground
                      :background nano-color-background
                      :weight 'regular
                      :height (if (display-graphic-p) 120 1)
                      :box `(:line-width 1
                                         :color ,nano-color-foreground
                                         :style nil))
  (set-face-attribute 'nano-face-tag-strong nil
                      :foreground nano-color-strong
                      :background nano-color-subtle
                      :weight 'regular
                      :height (if (display-graphic-p) 120 1)
                      :box `(:line-width 1
                                         :color ,nano-color-strong
                                         :style nil))
  (set-face-attribute 'nano-face-tag-salient nil
                      :foreground nano-color-background
                      :background nano-color-salient
                      :weight 'regular
                      :height (if (display-graphic-p) 120 1)
                      :box `(:line-width 1
                                         :color ,nano-color-salient
                                         :style nil))
  (set-face-attribute 'nano-face-tag-popout nil
                      :foreground nano-color-background
                      :background nano-color-popout
                      :weight 'regular
                      :height (if (display-graphic-p) 120 1)
                      :box `(:line-width 1
                                         :color ,nano-color-popout
                                         :style nil))
  (set-face-attribute 'nano-face-tag-faded nil
                      :foreground nano-color-background
                      :background nano-color-faded
                      :weight 'regular
                      :height (if (display-graphic-p) 120 1)
                      :box `(:line-width 1
                                         :color ,nano-color-faded
                                         :style nil))
  (set-face-attribute 'nano-face-tag-critical nil
                      :foreground nano-color-background
                      :background nano-color-critical
                      :weight 'regular
                      :height (if (display-graphic-p) 120 1)
                      :box `(:line-width 1
                                         :color ,nano-color-critical
                                         :style nil)))

;; TODO FIXME
;; cursot color
;; and font on macos
;; it seems nano face redefine font to roboto
(defun my/load-nano ()
  (interactive)
  (cond ((eq footheme 'nano-light) (require 'nano-theme-light))
        ((eq footheme 'nano-dark)) (require 'nano-theme-dark))

  (when (called-interactively-p 'any)
    (require 'nano-theme-light))

  (require 'nano-faces)
  (nano-faces)
  (redefine-nano-face-font-lock)

  (require 'nano-theme)
  (nano-theme--basics)
  (nano-theme--font-lock)
  (nano-theme--minibuffer)
  (nano-theme--buttons)
  (nano-theme--info)
  (nano-theme--bookmark)
  (nano-theme--message)
  (nano-theme--outline)
  (nano-theme--customize)
  (nano-theme--ido)
  (nano-theme--diff)
  (nano-theme--term)
  (nano-theme--calendar)
  (nano-theme--agenda)
  (nano-theme--org)
  (nano-theme--mu4e)
  (nano-theme--elfeed)
  (nano-theme--deft)
  (nano-theme--rst)
  (nano-theme--markdown)
  (nano-theme--hl-line)
  (nano-theme--company)

  (defun nano-theme--mode-line ()
    "Override nano func to setup mode line"
    (set-face-attribute 'mode-line nil
                        :foreground nano-color-foreground
                        :background nano-color-highlight
                        :box `(:line-width 1
                                           :color ,nano-color-background
                                           :style nil))
    (set-face-attribute 'mode-line-inactive nil
                        :foreground nano-color-foreground
                        :background nano-color-subtle
                        :box `(:line-width 1
                                           :color ,nano-color-background
                                           :style nil)))
  (nano-theme--mode-line)

  (custom-set-faces
   `(meow-keypad-indicator ((t (:inherit nano-face-header-popout))))
   `(meow-insert-indicator ((t (:inherit nano-face-header-critical))))
   `(meow-normal-indicator ((t (:inherit nano-face-header-faded))))
   `(meow-motion-indicator ((t (:inherit nano-face-header-popout))))))

(provide 'init-nano)
