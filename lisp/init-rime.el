;; -*- lexical-binding: t; -*-

(leaf rime
  :straight
  (rime :type git
        :host github
        :repo "DogLooksGood/emacs-rime"
        :files ("*.el" "Makefile" "lib.c"))
  :commands (toggle-input-method)
  :bind
  ((:rime-active-mode-map
    ("<tab>" . rime-inline-ascii))
   (:rime-mode-map
    ("C-`" . rime-send-keybinding)
    ("M-j" . rime-force-enable)))
  :custom
  (rime-cursor . "|")
  (rime-title . "rime")
  (default-input-method . "rime")
  (rime-inline-ascii-holder . ?a)
  (rime-show-candidate . 'minibuffer)
  (rime-translate-keybindings . '("C-f" "C-b" "C-n" "C-p" "C-g"))
  (rime-disable-predicates . '(meow-normal-mode-p
                               meow-motion-mode-p
                               meow-keypad-mode-p))
  (rime-inline-predicates . '(rime-predicate-space-after-cc-p
                              rime-predicate-current-uppercase-letter-p))
  `(rime-librime-root . ,(cond ((eq system-type 'darwin) (expand-file-name "librime/dist" user-emacs-directory))
                               (t rime-librime-root)))
  `(rime-user-data-dir . ,(cond ((eq system-type 'darwin) "~/Library/Rime")
                                ((eq system-type 'gnu/linux) "~/.config/fcitx/rime"))))

(provide 'init-rime)
