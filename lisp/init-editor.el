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
(straight-use-package 'pretty-hydra)

;;; pretty-hydra
(defun +change-theme (theme)
  "Disable other themes and load new one"
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

;; FIXME
(pretty-hydra-define toggles-hydra (:title "Toggles" :quit-key "q")
      ("Basic"
       (("n" (if (fboundp 'display-line-numbers-mode)
                 (display-line-numbers-mode (if display-line-numbers-mode -1 1))
               (global-linum-mode (if global-linum-mode -1 1)))
         "line number"
         :toggle (or (bound-and-true-p display-line-numbers-mode) global-linum-mode))
        ("e" electric-pair-mode "electric pair" :toggle t)
        ("c" flyspell-mode "spell check" :toggle t)
        ("s" prettify-symbols-mode "pretty symbol" :toggle t)
        ("b" display-battery-mode "battery" :toggle t)
        ("i" display-time-mode "time" :toggle t))
       "Highlight"
       (("h l" global-hl-line-mode "line" :toggle t)
        ("h p" show-paren-mode "paren" :toggle t)
        ("h r" rainbow-mode "rainbow" :toggle t)
        ("h d" rainbow-delimiters-mode "delimiter" :toggle t)
        ("h t" global-hl-todo-mode "todo" :toggle t))
       "Program"
       (("F" flymake-mode "flymake" :toggle t)
        ("O" hs-minor-mode "hideshow" :toggle t)
        ("u" subword-mode "subword" :toggle t)
        ("W" which-function-mode "which function" :toggle t)
        ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
        ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit))
        ("v" global-diff-hl-mode "gutter" :toggle t)
        ("V" diff-hl-flydiff-mode "live gutter" :toggle t)
        ("M" diff-hl-margin-mode "margin gutter" :toggle t)
        ("D" diff-hl-dired-mode "dired gutter" :toggle t))
       "Theme"
       (("t s" (+change-theme 'spacemacs-light) "space-light" :toggle t)
        ("t S" (+change-theme 'spacemacs-dark) "space-dark" :toggle t)
        ("t j" (+change-theme 'joker) "joker" :toggle t)
        ("t p" (+change-theme 'printed) "printed" :toggle t)
        ("t b" (+change-theme 'storybook) "storybook" :toggle t)
        ("t m" (+change-theme 'modus-vivendi) "modus-dark" :toggle t)
        ("t M" (+change-theme 'modus-operandi) "modus-light" :toggle t)
        ("t n" (+change-theme 'nasy) "nasy" :toggle t))))

(pretty-hydra-define tab-bar (:title "Tab Bar Operations" :quit-key "q")
  ("Options"
   (("n" tab-bar-new-tab "New")
    ("r" tab-rename "Rename")
    ("0" tab-bar-close-tab "Close current")
    ("1" tab-bar-close-other "Close other")
    ("u" tab-undo "Undo"))
   "Other Tab"
   (("b" switch-to-buffer-other-tab "Switch buffer")
    ("f" find-file-other-tab "Find file")
    ("d" dired-other-tab "Dired"))
   "Select"
   (("t" tab-bar-select-tab-by-name "Select by name")
    ("l" tab-next "Next Tab")
    ("h" tab-previous "Previous Tab"))))

(global-set-key (kbd "C-x t") 'tab-bar/body)
(global-set-key (kbd "C-h C-h") 'toggles-hydra/body)

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
 treemacs-no-png-images t
 treemacs-width 30)

(autoload #'treemacs "treemacs")
(autoload #'treemacs-select-window "treemacs")

(global-set-key (kbd "<f1>") 'treemacs-select-window)

(with-eval-after-load "treemacs"
  (define-key treemacs-mode-map (kbd "<f1>") 'treemacs)
  (add-hook 'treemacs-mode-hook #'+treemacs-scale-font-size))

(provide 'init-editor)
