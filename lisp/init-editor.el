;;; -*- lexical-binding: t -*-

(straight-use-package '(vundo :type git :host github :repo "casouri/vundo"))
(straight-use-package '(insert-translated-name :type git :host github :repo "manateelazycat/insert-translated-name"))
(straight-use-package 'markdown-mode)
(straight-use-package 'treemacs)
(straight-use-package '(auto-save :type git :host github :repo "manateelazycat/auto-save"))
(straight-use-package 'insert-char-preview)
(straight-use-package 'ibuffer-vc)
(straight-use-package 'visual-fill-column)
(straight-use-package 'separedit)
(straight-use-package 'imenu-list)
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'which-key)
(straight-use-package 'writeroom-mode)
(straight-use-package 'youdao-dictionary)

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
 imenu-list-auto-resize t)

(global-set-key (kbd "C-.") #'imenu-list-smart-toggle)

;;; separedit
(setq
 separedit-default-mode 'org-mode
 separedit-remove-trailing-spaces-in-comment t)

(autoload #'separedit "separedit" nil t)

(global-set-key (kbd "C-c '") #'separedit)

;;; visual-fill-column
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)

;; ibuffer-vc
(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              " "
              vc-relative-file)))

(with-eval-after-load "ibuffer"
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

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

;; markdown-mode
(setq markdown-fontify-code-blocks-natively t)
(add-hook 'markdown-mode-hook 'markdown-toggle-markup-hidding)

;;; treemacs
(defun +treemacs-scale-font-size ()
  (face-remap-add-relative 'default :height 0.8))

(setq
 treemacs-user-mode-line-format 'none
 treemacs-no-png-images t
 treemacs-width 30)

(autoload #'treemacs "treemacs")
(autoload #'treemacs-select-window "treemacs")

(global-set-key (kbd "<f1>") 'treemacs-select-window)

(with-eval-after-load "treemacs"
  (define-key treemacs-mode-map (kbd "<f1>") 'treemacs)
  (add-hook 'treemacs-mode-hook #'+treemacs-scale-font-size))

(provide 'init-editor)
