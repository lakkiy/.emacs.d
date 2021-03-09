;; -*- lexical-binding: t; -*-

(leaf rime
  :straight
  (rime :type git
        :host github
        :repo "DogLooksGood/emacs-rime"
        :files ("*.el" "Makefile" "lib.c"))
  :commands (toggle-input-method)
  :init
  (if (eq system-type 'darwin)
      (setq rime-librime-root (expand-file-name "librime/dist" user-emacs-directory)))
  :custom
  (default-input-method . "rime")
  (rime-cursor . "|")
  (rime-title . "rime")
  (rime-show-candidate . 'minibuffer)
  (rime-translate-keybindings . '("C-f" "C-b" "C-n" "C-p" "C-g"))
  (rime-disable-predicates . '(meow-normal-mode-p
                               meow-motion-mode-p
                               meow-keypad-mode-p))
  `(rime-user-data-dir . ,(cond ((eq system-type 'darwin) "~/Library/Rime")
                                ((eq system-type 'gnu/linux) "~/.config/fcitx/rime"))))

(provide 'init-rime)
