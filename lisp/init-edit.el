;;; -*- lexical-binding: t -*-

(eat-package mode-line-bell
  :straight t
  :hook (after-init-hook . mode-line-bell-mode))

(eat-package symbol-overlay
  :straight t
  :hook
  ((prog-mode-hook conf-mode-hook) . symbol-overlay-mode)
  :config
  (define-key symbol-overlay-mode-map (kbd "M-i") 'symbol-over-put)
  (define-key symbol-overlay-mode-map (kbd "M-I") 'symbol-overlay-remove-all)
  (define-key symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
  (define-key symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev))

(eat-package anzu
  :straight t
  :init
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

(eat-package separedit
  :straight t
  :init
  ;; use C-u C-c ' to select major mode
  (global-set-key (kbd "C-c '") #'separedit))

(eat-package puni
  :straight (puni :type git :host github :repo "AmaiKinono/puni")
  :hook (emacs-lisp-mode-hook . puni-mode)
  :config
  (define-key puni-mode-map (kbd "M-r") 'puni-splice)
  (define-key puni-mode-map (kbd "C-(") 'puni-slurp-backward)
  (define-key puni-mode-map (kbd "C-)") 'puni-slurp-forward)
  (define-key puni-mode-map (kbd "C-{") 'puni-barf-backward)
  (define-key puni-mode-map (kbd "C-}") 'puni-barf-forward))

(eat-package treemacs
  :straight ( treemacs
              :files (:defaults "icons"
                                "src/elisp/treemacs*.el"
                                "src/scripts/treemacs*.py"
                                "src/extra/*")
              :includes (treemacs-all-the-icons
                         treemacs-icons-dired
                         treemacs-magit))
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

;; Better scroll on picture in GUI
(eat-package iscroll :straight t)

(eat-package fanyi
  :straight (fanyi :type git :host github :repo "condy0919/fanyi.el")
  :hook (fanyi-mode-hook . visual-line-mode)
  :init
  (setq fanyi-verbose nil)
  (global-set-key (kbd "C-c y") 'fanyi-dwim))

(eat-package visual-fill-column
  :straight t
  ;; HACK Add `visuall-fill-column-mode' to `visual-line-mode-hook' will break `olivetti-mode' center window
  :hook (visual-fill-column-mode-hook . visual-line-mode))

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

(eat-package undo-hl
  :straight (undo-hl :type git :host github :repo "casouri/undo-hl")
  :hook
  ((prog-mode-hook conf-mode-hook) . undo-hl-mode))

(eat-package avy
  :straight t
  :init
  (global-set-key (kbd "C-'") #'avy-goto-char-2)
  :config
  (setq avy-background t
        avy-style 'pre))

(eat-package ligature
  :straight (ligature :type git :host github :repo "mickeynp/ligature.el")
  :commands global-ligature-mode
  :hook (after-init-hook . (lambda () (global-ligature-mode t)))
  :config
  ;; https://htmlpreview.github.io/?https://github.com/kiliman/operator-mono-lig/blob/master/images/preview/normal/index.html
  (ligature-set-ligatures 'prog-mode
                          '("&&" "||" "|>" ":=" "==" "===" "==>" "=>"
                            "=<<" "!=" "!==" ">=" ">=>" ">>=" "->" "--"
                            "-->" "<|" "<=" "<==" "<=>" "<=<" "<!--" "<-"
                            "<->" "<--" "</" "+=" "++" "??" "/>" "__" "WWW")))

(eat-package hl-todo
  :straight t
  :hook
  ((dired-mode-hook prog-mode-hook conf-mode-hook) . hl-todo-mode))

(eat-package which-key
  :straight t
  :hook (after-init-hook . which-key-mode)
  :init
  (setq-default which-key-idle-delay 1.5))

(eat-package imenu-list
  :straight t
  :hook
  (imenu-list-major-mode-hook . (lambda ()
                                  (setq-local header-line-format nil)))
  :init
  (defun +imenu-scale-font-size ()
    (face-remap-add-relative 'default :height 0.8))
  (add-hook 'imenu-list-major-mode-hook #'+imenu-scale-font-size)
  (setq imenu-list-auto-resize t
        imenu-list-mode-line-format nil))


(provide 'init-edit)
