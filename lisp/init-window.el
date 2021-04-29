;;; -*- lexical-binding: t -*-

(setq
 tab-bar-show nil
 tab-bar-new-tab-choice "*scratch*")

(global-set-key (kbd "C-c M-t t") 'tab-bar-mode)
(global-set-key (kbd "C-c M-t r") 'tab-bar-rename-tab)
(global-set-key (kbd "C-c M-t n") 'tab-bar-new-tab)
(global-set-key (kbd "C-c M-t d") 'tab-bar-close-tab)



(straight-use-package 'yascroll)

(add-hook 'prog-mode-hook 'yascroll-bar-mode)
(add-hook 'conf-mode-hook 'yascroll-bar-mode)



(straight-use-package 'ace-window)

(setq
 aw-keys '(?a ?o ?e ?u ?i)
 aw-scope 'frame)

(autoload 'ace-swap-window "ace-window")
(autoload 'ace-window "ace-window")

;; TODO customize face

(provide 'init-window)
