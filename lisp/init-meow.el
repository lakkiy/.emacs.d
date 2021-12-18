;;; -*- lexical-binding: t -*-

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
  (meow-setup)
  :init
  (setq meow-esc-delay 0.001)
  (cond ((eq +meow-layout 'dvorak) (require 'init-meow-dvorak))
        (t (require 'init-meow-qwerty))))

(provide 'init-meow)
