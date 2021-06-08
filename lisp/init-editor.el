;;; -*- lexical-binding: t -*-

(straight-use-package '(vundo :type git :host github :repo "casouri/vundo"))
(straight-use-package '(insert-translated-name :type git :host github :repo "manateelazycat/insert-translated-name"))
(straight-use-package '(auto-save :type git :host github :repo "manateelazycat/auto-save"))
(straight-use-package 'insert-char-preview)
(straight-use-package 'visual-fill-column)
(straight-use-package 'separedit)
(straight-use-package 'imenu-list)
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'which-key)
(straight-use-package 'writeroom-mode)
(straight-use-package 'youdao-dictionary)
(straight-use-package 'dumb-jump)
(straight-use-package 'iscroll)
(straight-use-package 'good-scroll)

(+pdump-packages 'vundo
                 'good-scroll
                 'iscroll
                 'insert-char-preview
                 'auto-save
                 'insert-translated-name
                 'visual-fill-column
                 'separedit
                 'imenu-list
                 'exec-path-from-shell
                 'which-key
                 'writeroom-mode
                 'dumb-jump
                 'youdao-dictionary)

;;; good-scroll : Good pixel line scrolling
(add-hook 'after-init-hook 'good-scroll-mode)

(with-eval-after-load "good-scroll"
  (global-set-key [next] #'good-scroll-up-full-screen)
  (global-set-key [prior] #'good-scroll-down-full-screen))

;;; iscroll : Smooth scrolling over images
(add-hook 'eww-mode-hook 'iscroll-mode)

;;; dumb-jump
(setq
 dumb-jump-quiet t
 dumb-jump-aggressive t
 dumb-jump-prefer-search 'rg
 dumb-jump-selector 'completing-read
 dumb-jump-disable-obsolate-warning t)

(global-set-key (kbd "M-g J") 'dumb-jump-go-other-window)
(global-set-key (kbd "M-g j") 'dumb-jump-go)

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate t)

;;; youdao-dictionary
(setq url-automatic-caching t
      youdao-dictionary-search-history-file (expand-file-name ".youdao" user-emacs-directory)
      youdao-dictionary-use-chinese-word-segmentation t)

(pretty-hydra-define youdao-hydra (:title "Youdao Dictionary Operations" :quit-key "q")
  ("Operations"
   (("p" youdao-dictionary-play-voice-of-current-word "play voice of current word")
    ("y" youdao-dictionary-play-voice-at-point "play voice at point"))))

(global-set-key (kbd "C-c y") 'youdao-dictionary-search-at-point)
(global-set-key (kbd "C-c Y") 'youdao-dictionary-search-at-point+)

(with-eval-after-load "youdao-dictionary"
  (add-hook 'youdao-dictionary-mode-hook (lambda () (meow-mode -1)))
  (define-key youdao-dictionary-mode-map (kbd "h") 'youdao-hydra/body)
  (define-key youdao-dictionary-mode-map (kbd "?") 'youdao-hydra/body))

;;; writeroom-mode
(setq
 writeroom-fullscreen-effect 'maximized
 writeroom-bottom-divider-width 0)

;; which-key
(setq
 which-key-idle-delay 1
 which-key-idle-secondary-delay 0.05)

(add-hook 'after-init-hook 'which-key-mode)

(with-eval-after-load "which-key"
  (global-set-key (kbd "<f5>") 'which-key-show-major-mode))

;; exec-path-from-shell
(when (memq window-system '(mac ns x))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;;; imenu-list
(setq
 imenu-list-auto-resize t
 imenu-list-mode-line-format nil)

(global-set-key (kbd "C-.") #'imenu-list-smart-toggle)

(with-eval-after-load "imenu-list"
  ;; disable header line in imenu list
  (add-hook 'imenu-list-major-mode-hook (lambda ()
                                          (setq-local header-line-format nil))))

;;; separedit
(setq
 separedit-default-mode 'org-mode
 separedit-remove-trailing-spaces-in-comment t)

(autoload #'separedit "separedit" nil t)

(global-set-key (kbd "C-c '") #'separedit)

;;; visual-fill-column
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)

;; insert-char-preview
(autoload 'insert-char-preview "insert-char-preview" nil t)

;; auto-save
(setq auto-save-silent t
      auto-save-idle 3)

(require 'auto-save)
(auto-save-enable)

;; vundo
(autoload 'vundo "vundo" nil t)

;; company-english-helper
(autoload 'toggle-company-english-helper "company-english-helper" nil t)

;; insert-translated-name
(autoload 'insert-translated-name-insert "insert-translated-name" nil t)

(global-set-key (kbd "C-c i") 'insert-translated-name-insert)

(provide 'init-editor)
