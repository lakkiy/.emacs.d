;;; -*- lexical-binding: t -*-

(eat-package anzu
  :straight t
  :init
  ;; FIXME display anzu on modeline
  (setq anzu-cons-mode-line-p nil)
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
  (define-key puni-mode-map (kbd "M-r") 'puni-raise)
  (define-key puni-mode-map (kbd "M-)") 'puni-splice)
  (define-key puni-mode-map (kbd "C-(") 'puni-slurp-backward)
  (define-key puni-mode-map (kbd "C-)") 'puni-slurp-forward)
  (define-key puni-mode-map (kbd "C-{") 'puni-barf-backward)
  (define-key puni-mode-map (kbd "C-}") 'puni-barf-forward)
  (dolist (key '("M-[" "M-{" "M-(" "M-\""))
    (define-key puni-mode-map (kbd key) 'puni-squeeze)))

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

(eat-package sdcv
  :straight (sdcv :type git :host github :repo "manateelazycat/sdcv")
  :commands
  sdcv-search-pointer
  sdcv-search-pointer+
  sdcv-search-input
  sdcv-search-input+
  :init
  (setq sdcv-dictionary-data-dir (file-truename "~/.sdcv-dict")
        sdcv-dictionary-simple-list
        '("懒虫简明英汉词典"
          "懒虫简明汉英词典"
          "KDic11万英汉词典")
        sdcv-dictionary-complete-list
        '("懒虫简明英汉词典"
          "英汉汉英专业词典"
          "XDICT英汉辞典"
          "stardict1.3英汉辞典"
          "WordNet"
          "XDICT汉英辞典"
          "懒虫简明汉英词典"
          "新世纪英汉科技大词典"
          "KDic11万英汉词典"
          "朗道汉英字典5.0"
          "CDICT5英汉辞典"
          "新世纪汉英科技大词典"
          "牛津英汉双解美化版"
          "21世纪双语科技词典"
          "quick_eng-zh_CN"))
  (defun sdcv-dwim (word)
    "Translate WORD."
    (interactive (let* ((default (if (use-region-p)
                                     (buffer-substring-no-properties (region-beginning) (region-end))
                                   (thing-at-point 'word t)))
                        (prompt (if (stringp default)
                                    (format "Search Word (default \"%s\"): " default)
                                  "Search Word: ")))
                   (list (read-string prompt nil nil default))))
    (sdcv-search-input word))
  (global-set-key (kbd "C-c Y") #'sdcv-dwim))

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
        imenu-list-mode-line-format nil)

  (global-set-key (kbd "C-.") #'imenu-list-smart-toggle))

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

(eat-package avy
  :straight t
  :init
  (global-set-key (kbd "C-'") #'avy-goto-char-2)
  :config
  (setq avy-background t
        avy-style 'pre))

(provide 'init-edit)
