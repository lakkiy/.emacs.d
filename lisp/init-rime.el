;;; -*- lexical-binding: t -*-

(eat-package rime
  :straight t
  :commands toggle-input-method
  :init
  (setq rime-disable-predicates '(meow-normal-mode-p
                                  meow-motion-mode-p
                                  meow-keypad-mode-p)
        ;; rime-inline-predicates '(rime-predicate-space-after-cc-p
        ;;                          rime-predicate-current-uppercase-letter-p)
        rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g")
        rime-inline-ascii-holder ?a
        default-input-method "rime"
        rime-cursor "|"
        rime-show-candidate 'sidewindow
        window-min-height 1
        rime-title "ㄓ ")
  (if sys/macp
      (setq rime-librime-root (expand-file-name "librime/dist" user-emacs-directory)
            rime-user-data-dir "~/Library/Rime"))
  (if sys/linuxp
      (setq rime-user-data-dir "~/.config/fcitx/rime"))
  :config
  (define-key rime-active-mode-map [tab] 'rime-inline-ascii)
  (define-key rime-mode-map (kbd "C-`") 'rime-send-keybinding)
  (define-key rime-mode-map (kbd "M-j") 'rime-force-enable))

(provide 'init-rime)
