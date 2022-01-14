;;; -*- lexical-binding: t -*-

(setq-default header-line-format nil)
(setq-default frame-title-format "⚘ %f ⚘")

;; TODO process icon in terminal and check +icons-p
(eat-package telephone-line
  :straight t
  :hook (after-init-hook . telephone-line-mode)
  :init
  (setq-default mode-line-format nil)
  (require 'telephone-segments)

  ;; Set separator style
  (setq telephone-line-primary-left-separator 'telephone-line-halfsin-left)
  (setq telephone-line-primary-right-separator 'telephone-line-halfsin-right)

  ;; Set mode-line height
  (setq telephone-line-height 28)

  ;; Left edge
  ;; meow project-buffer
  ;; TODO: gray background for buffer and mode segment in inactive line
  (setq telephone-line-lhs
        '((accent . ((my-meow-segment :active)))
          (nil . (my-project-buffer-segment))
          ))
  ;; (nil    . (my-flycheck-segment))))

  ;; Right edge
  ;; vc pos major encoding
  (setq telephone-line-rhs
        '((nil    . (my-vc-segment))
          (accent . ((my-position-segment :active)))
          (nil    . ((my-major-mode-segment-icon :active)))
          (accent . ((my-coding-segment :active)))
          ))

  ;; TODO rime flymake/flycheck anzu
  )

(provide 'init-modeline)
