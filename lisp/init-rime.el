;; -*- lexical-binding: t; -*-

;; TODO set
;; rime-share-data-dir
;; rime-user-data-dir
;; for mac and linux
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
  :pre-setq
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
                              rime-predicate-current-uppercase-letter-p)))

(provide 'init-rime)
