;;; -*- lexical-binding: t -*-

(eat-package popper
  :straight t
  :commands
  popper-toggle-latest
  popper-cycle
  :hook (after-init-hook . popper-mode)
  :init
  (+pdump-packages 'popper)

  (global-set-key (kbd "C-`") 'popper-toggle-latest)
  (global-set-key (kbd "M-`") 'popper-cycle)

  (setq popper-modeline nil
        popper-mode-line nil
        popper-reference-buffers
        '("\\*Messages\\*"
          "Outout\\*$"
          "\\*Gofmt Errors\\*"
          help-mode
          eshell-mode
          ielm-mode)
        popper-group-function #'popper-group-by-directory))

(eat-package sr-speedbar
  :straight t
  :init
  (setq sr-speedbar-auto-refresh nil
        sr-speedbar-default-width 30
        sr-speedbar-max-width 40
        sr-speedbar-right-side nil
        sr-speedbar-skip-other-window-p t)
  (defun +sr-speedbar-select-or-open()
    "Select sr-speedbar window if it exist, or open sr-speedbar"
    (interactive)
    (if (and (fboundp 'sr-speedbar-exist-p) (sr-speedbar-exist-p))
        (sr-speedbar-select-window)
      (sr-speedbar-open)))

  (global-set-key (kbd "<f1>") #'+sr-speedbar-select-or-open)
  :hook
  (speedbar-mode-hook . (lambda ()
                          (face-remap-add-relative 'default :height 0.8)
                          (face-remap-add-relative 'hl-line :box '(:line-width (-1 . -1)))))
  :config
  (defun speedbar-set-mode-line-format ()
    "Override function to disable mode line and header line in speedbar"
    (setq mode-line-format nil)
    (setq header-line-format nil))
  (define-key speedbar-mode-map (kbd "<f1>") 'sr-speedbar-close))

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

(eat-package writeroom-mode
  :straight t
  :init
  (setq writeroom-fullscreen-effect 'maximized
        writeroom-bottom-divider-width 0))

(eat-package which-key
  :straight t
  :hook (after-init-hook . which-key-mode)
  :init
  (setq which-key-idle-delay 1
        which-key-idle-secondary-delay 0.05)
  :config
  (global-set-key (kbd "<f5>") 'which-key-show-major-mode))

(eat-package exec-path-from-shell
  :straight t
  :init
  (when (memq window-system '(mac ns x))
    (eat-package cache-path-from-shell
      :straight (cache-path-from-shell :type git :host github :repo "manateelazycat/cache-path-from-shell")
      :require t)
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

(eat-package separedit
  :straight t
  :commands separedit
  :init
  (setq separedit-default-mode 'org-mode
        separedit-remove-trailing-spaces-in-comment t)
  (global-set-key (kbd "C-c '") #'separedit))

(eat-package visual-fill-column
  :straight t
  :hook (visual-line-mode-hook . visual-fill-column-mode))

(eat-package insert-char-preview
  :straight t
  :commands insert-char-preview)

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

(eat-package helpful
  :straight t
  :init
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function)
  (global-set-key (kbd "C-h C") #'helpful-command))

(eat-package elisp-demos
  :straight t
  :init
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(provide 'init-editor)
