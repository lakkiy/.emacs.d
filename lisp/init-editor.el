;;; -*- lexical-binding: t -*-

(eat-package treemacs
  :straight t
  :init
  (defun +treemacs-scale-font-size ()
    (face-remap-add-relative 'default :height 0.8))
  (setq treemacs-no-png-images t
        treemacs-width 30
        treemacs-user-mode-line-format 'none)
  (global-set-key (kbd "<f1>") 'treemacs-select-window)
  :config
  (define-key treemacs-mode-map (kbd "<f1>") 'treemacs)
  (add-hook 'treemacs-mode-hook #'+treemacs-scale-font-size))

(eat-package good-scroll
  :straight t
  :hook (after-init-hook . good-scroll-mode)
  :config
  (global-set-key [next] #'good-scroll-up-full-screen)
  (global-set-key [prior] #'good-scroll-down-full-screen))

(eat-package iscroll
  :straight t
  :hook (eww-mode-hook . iscroll-mode))

(eat-package fanyi
  :straight (fanyi :type git :host github :repo "condy0919/fanyi.el")
  :hook (fanyi-mode-hook . visual-line-mode)
  :init
  (global-set-key (kbd "C-c y") 'fanyi-dwim))

(eat-package exec-path-from-shell
  :straight t
  :init
  (when (eq system-type 'darwin)
    (add-hook 'after-init-hook #'exec-path-from-shell-initialize)))

(eat-package imenu-list
  :straight t
  :hook
  (imenu-list-major-mode-hook . (lambda ()
                                  (setq-local header-line-format nil)))
  :init
  (setq imenu-list-auto-resize t
        imenu-list-mode-line-format nil)

  (global-set-key (kbd "C-.") #'imenu-list-smart-toggle))

(eat-package visual-fill-column
  :straight t
  :hook (visual-line-mode-hook . visual-fill-column-mode))

(eat-package auto-save
  :straight (auto-save :type git :host github :repo "manateelazycat/auto-save")
  :init
  (setq
   auto-save-silent t
   auto-save-idle 3)
  :require t
  :config
  (auto-save-enable))

(eat-package vundo
  :straight (vundo :type git :host github :repo "casouri/vundo")
  :commands vundo)

(eat-package insert-translated-name
  :straight (insert-translated-name :type git
                                    :host github
                                    :repo "manateelazycat/insert-translated-name")
  :commands insert-translated-name-insert
  :init
  (global-set-key (kbd "C-c i") 'insert-translated-name-insert))

(eat-package elisp-demos
  :straight t
  :init
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(provide 'init-editor)
