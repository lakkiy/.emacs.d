;;; -*- lexical-binding: t -*-

(eat-package good-scroll
  :straight t
  :hook (after-init-hook . good-scroll-mode)
  :config
  (global-set-key [next] #'good-scroll-up-full-screen)
  (global-set-key [prior] #'good-scroll-down-full-screen))

(eat-package iscroll
  :straight t
  :hook (eww-mode-hook . iscroll-mode))

(eat-package youdao-dictionary
  :straight t
  :hook (youdao-dictionary-mode-hook . (lambda () (meow-mode -1)))
  :init
  (setq url-automatic-caching t
        youdao-dictionary-search-history-file (expand-file-name ".youdao" user-emacs-directory)
        youdao-dictionary-use-chinese-word-segmentation t)
  (pretty-hydra-define youdao-hydra (:title "Youdao Dictionary Operations" :quit-key "q")
    ("Operations"
     (("p" youdao-dictionary-play-voice-of-current-word "play voice of current word")
      ("y" youdao-dictionary-play-voice-at-point "play voice at point"))))

  (global-set-key (kbd "C-c y") 'youdao-dictionary-search-at-point-posframe)
  (global-set-key (kbd "C-c Y") 'youdao-dictionary-search-at-point)
  :config
  (define-key youdao-dictionary-mode-map (kbd "h") 'youdao-hydra/body)
  (define-key youdao-dictionary-mode-map (kbd "?") 'youdao-hydra/body))

(eat-package exec-path-from-shell
  :straight t
  :init
  (when (memq window-system '(mac ns x))
    (require 'exec-path-from-shell)
    (exec-path-from-shell-initialize)))

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
