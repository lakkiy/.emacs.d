;;; -*- lexical-binding: t -*-

(straight-use-package '(vundo :type git :host github :repo "casouri/vundo"))
(straight-use-package '(insert-translated-name :type git :host github :repo "manateelazycat/insert-translated-name"))
(straight-use-package '(company-english-helper :type git :host github :repo "manateelazycat/company-english-helper"))
(straight-use-package 'markdown-mode)
(straight-use-package 'go-translate)
(straight-use-package 'treemacs)
(straight-use-package '(auto-save :type git :host github :repo "manateelazycat/auto-save"))
(straight-use-package 'insert-char-preview)
(straight-use-package 'ibuffer-vc)
(straight-use-package 'visual-fill-column)
(straight-use-package 'olivetti)

;;; olivetti
(setq
 olivetti-body-width 80)

(autoload 'olivetti-mode "olivetti" nil t)

;;; visual-fill-column
(with-eval-after-load "visual-line"
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))

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

;; markdown-mode
(setq markdown-fontify-code-blocks-natively t)
(add-hook 'markdown-mode-hook 'markdown-toggle-markup-hidding)

;; go-translate
(setq go-translate-base-url "https://translate.google.cn")
(setq go-translate-local-language "zh-CN")
(setq go-translate-inputs-function #'go-translate-inputs-current-or-prompt)
(setq go-translate-token-current (cons 430675 2721866130))

;; treemacs
(defun +treemacs-scale-font-size ()
  (face-remap-add-relative 'default :height 0.8))

(setq
 treemacs-no-png-images t
 treemacs-width 30)

(autoload #'treemacs "treemacs")
(autoload #'treemacs-select-window "treemacs")

(global-set-key (kbd "<f1>") 'treemacs-select-window)

(with-eval-after-load "treemacs"
  (define-key treemacs-mode-map (kbd "<f1>") 'treemacs)
  (add-hook 'treemacs-mode-hook #'+treemacs-scale-font-size))

(provide 'init-editor)
