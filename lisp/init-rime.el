;;; -*- lexical-binding: t -*-

;; TODO * - = 等直接上屏，或者在 org mode 中
(eat-package rime
  :straight t
  :commands toggle-input-method
  :init
  (setq rime-disable-predicates '(meow-normal-mode-p
                                  meow-motion-mode-p
                                  meow-keypad-mode-p)
        rime-inline-predicates '(rime-predicate-space-after-cc-p
                                 rime-predicate-current-uppercase-letter-p)
        rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g" "C-v" "M-v")
        rime-inline-ascii-holder ?a
        default-input-method "rime"
        rime-cursor "|"
        rime-show-candidate 'minibuffer
        rime-title "ㄓ ")
  (when sys/macp
    (setq rime-librime-root (expand-file-name "librime/dist" user-emacs-directory)))
  :config
  (define-key rime-active-mode-map [tab] 'rime-inline-ascii)
  (define-key rime-mode-map (kbd "C-`") 'rime-send-keybinding)
  (define-key rime-mode-map (kbd "M-j") 'rime-force-enable))

(provide 'init-rime)
