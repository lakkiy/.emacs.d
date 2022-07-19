;;; -*- lexical-binding: t -*-

(eat-package anzu
  :straight t
  :init
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

(eat-package separedit
  :straight t
  :hook (separedit-mode-hook . (lambda () (auto-fill-mode 1)))
  :init
  ;; use C-u C-c ' to select major mode
  (global-set-key (kbd "C-c '") #'separedit))

(eat-package puni
  :straight (puni :type git :host github :repo "AmaiKinono/puni")
  :hook
  ((emacs-lisp-mode-hook scheme-mode-hook clojure-mode-hook)
   . puni-mode)
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

  (advice-add 'treemacs-select-window :after #'eat/pulse-momentary-line)
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
  (global-set-key (kbd "C-c Y") 'fanyi-dwim))

(eat-package visual-fill-column
  :straight t
  :hook (visual-fill-column-mode-hook . visual-line-mode)
  :init
  (setq-default visual-fill-column-center-text t))

(eat-package auto-save
  :straight (auto-save
             :type git
             :host github
             :repo "manateelazycat/auto-save")
  :commands auto-save-enable
  :hook (after-init-hook . (lambda ()
                             (auto-save-enable)))
  :init
  (setq
   auto-save-silent t
   ;; most time I will save manually
   auto-save-idle 7))

;;; undo things

(eat-package vundo
  :straight (vundo :type git :host github :repo "casouri/vundo")
  :commands vundo)

(eat-package undo-hl
  :straight (undo-hl :type git :host github :repo "casouri/undo-hl")
  :hook
  ((prog-mode-hook conf-mode-hook) . undo-hl-mode))

;;; Help

(eat-package ghelp
  :straight (ghelp :type git :host github :repo "casouri/ghelp")
  :commands
  ghelp-describe
  ghelp-describe-function
  ghelp-describe-variable
  ghelp-describe-key
  ghelp-describe-elisp
  :init
  (global-set-key (kbd "C-h C-h") #'ghelp-describe)
  (global-set-key (kbd "C-h f") #'ghelp-describe-function)
  (global-set-key (kbd "C-h v") #'ghelp-describe-variable)
  (global-set-key (kbd "C-h k") #'ghelp-describe-key)
  (global-set-key (kbd "C-h o") #'ghelp-describe-elisp)
  :config
  (global-set-key (kbd "C-h r") #'ghelp-resume))

;;;

(eat-package avy
  :straight t
  :init
  (global-set-key (kbd "C-'") #'avy-goto-char-timer)
  :config
  (setq avy-background t
        avy-style 'pre))

(eat-package ligature
  :straight (ligature :type git :host github :repo "mickeynp/ligature.el")
  :commands global-ligature-mode
  :hook (prog-mode-hook . (lambda () (ligature-mode t)))
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

;;; outline
(eat-package color-outline
  :commands color-outline-mode
  :hook (prog-mode-hook . color-outline-mode)
  :config
  ;; TODO /* ?
  (push '(go-mode "/") color-outline-comment-char-alist))

(eat-package go-translate
  :straight t
  :init
  (setq gts-translate-list '(("en" "zh")))
  (global-set-key (kbd "C-c y") #'gts-do-translate)
  :config
  (setq gts-default-translator (gts-translator
                                :picker (gts-noprompt-picker)
                                :engines (list (gts-bing-engine) (gts-google-rpc-engine))
                                :render (gts-buffer-render))))

;;; init-edit.el ends here
(provide 'init-edit)
