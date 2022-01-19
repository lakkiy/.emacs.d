;;; -*- lexical-binding: t -*-

(setq-default header-line-format nil)
(setq-default frame-title-format "⚘ %f ⚘")

;; TODO process icon in terminal and check +icons-p
(eat-package telephone-line
  :straight t
  :hook (after-init-hook . telephone-line-mode)
  :init
  (defvar modeline-height 17)
  ;; Set mode-line height
  (setq telephone-line-height modeline-height)

  (setq-default mode-line-format nil)
  (require 'telephone-segments)

  ;; Set separator style
  (setq telephone-line-primary-left-separator 'telephone-line-halfsin-left)
  (setq telephone-line-primary-right-separator 'telephone-line-halfsin-right)

  ;; Set subseparator
  ;; TODO: function to choose separator by name
  (when window-system
    (setq telephone-line-secondary-left-separator 'telephone-line-identity-hollow-left
          telephone-line-secondary-right-separator 'telephone-line-identity-hollow-right))

  ;; Left edge
  ;; meow project-buffer
  ;; TODO: gray background for buffer and mode segment in inactive line
  (setq telephone-line-lhs
        '((accent . ((my-meow-segment :active)))
          (nil . (my-project-buffer-segment))
          (nil . (my-modified-status-segment))
          (nil . (my-read-only-status-segment))
          ))
  ;; (nil    . (my-flycheck-segment))))

  ;; Right edge
  ;; vc pos major encoding
  (setq telephone-line-rhs
        '((nil    . (my-rime-segment))
          (nil    . (my-vc-segment))
          (accent . ((my-position-segment :active)))
          (nil    . ((my-major-mode-segment-icon :active)))
          (accent . ((my-coding-segment :active)))
          (nil    . (my-flymake-segment))
          ))

  ;; TODO rime flymake/flycheck anzu
  )

(provide 'init-modeline)
