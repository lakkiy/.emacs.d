;;; -*- lexical-binding: t -*-

(require 'init-meow-dvorak)
(require 'init-meow-qwerty)

(eat-package meow
  :straight t
  :hook
  (after-init-hook . (lambda ()
                       (meow-global-mode 1)))
  :config
  ;; SPC h f to describe-funtion
  (global-set-key (kbd "C-h C-f") 'describe-funtion)
  ;; normal mode list
  (dolist (mode '(go-dot-mod-mode
                  diff-mode))
    (add-to-list 'meow-mode-state-list `(,mode . normal)))
  ;; motion mode list
  (dolist (mode '(notmuch-hello-mode
                  notmuch-search-mode
                  notmuch-tree-mode))
    (add-to-list 'meow-mode-state-list `(,mode . motion)))
  (setq
   meow-replace-state-name-list '((normal . "N")
                                  (motion . "M")
                                  (keypad . "K")
                                  (insert . "I")
                                  (beacon . "B")))
  ;; setup meow with layout
  (cond ((eq +meow-layout 'dvorak) (meow-setup-dvorak))
        (t (meow-setup-qwerty)))
  :init
  (setq meow-esc-delay 0.001))

(provide 'init-meow)
