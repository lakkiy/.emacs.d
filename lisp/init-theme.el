;;; -*- lexical-binding: t -*-

(defvar +theme-hooks nil
  "((theme-id . function) ...)")
(defun +load-theme-advice (f theme-id &optional no-confirm no-enable &rest args)
  "Enhance `load-theme' by disabling other enabled themes & calling hooks"
  (unless no-enable ;
    (mapc #'disable-theme custom-enabled-themes))
  (prog1
      (apply f theme-id no-confirm no-enable args)
    (unless no-enable ;
      (pcase (assq theme-id +theme-hooks)
        (`(,_ . ,f) (funcall f))))))
(advice-add 'load-theme :around #'+load-theme-advice)

(eat-package nano-theme
  :init
  (setq nano-theme-light/dark 'light
        nano-theme-comment-italic nil
        nano-theme-keyword-italic nil
        nano-theme-overline-modeline t
        nano-theme-padded-modeline nil
        nano-theme-system-appearance t))

(eat-package doom-themes
  :straight t
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-padded-modeline t
        doom-spacegrey-brighter-comments t
        doom-spacegrey-brighter-modeline t
        doom-spacegrey-comment-bg t)
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(eat-package spacemacs-theme
  :straight t
  :init
  (setq spacemacs-theme-comment-italic t
        spacemacs-theme-keyword-italic t
        spacemacs-theme-org-agenda-height t
        spacemacs-theme-org-bold t
        spacemacs-theme-org-height t
        spacemacs-theme-org-highlight t
        spacemacs-theme-org-priority-bold t
        spacemacs-theme-org-bold t
        spacemacs-theme-underline-parens t))

(eat-package kaolin-themes
  :straight t
  :init
  (setq kaolin-themes-underline-wave nil
        kaolin-themes-modeline-border nil
        kaolin-themes-modeline-padded 4))

(defvar +theme 'kaolin-aurora
  "Theme used in GUI.")
(defvar +theme-tui 'minidark
  "Theme used in TUI.")

(eat-package ligature
  :straight (ligature :type git :host github :repo "mickeynp/ligature.el")
  :commands global-ligature-mode
  :config
  ;; https://htmlpreview.github.io/?https://github.com/kiliman/operator-mono-lig/blob/master/images/preview/normal/index.html
  (ligature-set-ligatures 'prog-mode
                          '("&&" "||" "|>" ":=" "==" "===" "==>" "=>"
                            "=<<" "!=" "!==" ">=" ">=>" ">>=" "->" "--"
                            "-->" "<|" "<=" "<==" "<=>" "<=<" "<!--" "<-"
                            "<->" "<--" "</" "+=" "++" "??" "/>" "__" "WWW")))

(defvar +font-default "Rec Mono Casual")
(defvar +font-size 13)
(defvar +font-unicode "Apple Color Emoji")
(defvar +font-cn "WenQuanYi Micro Hei")
(defvar +font-variable-pitch "Cardo")
(defvar +font-fixed-pitch "Recursive")

(defun +load-base-font ()
  (let* ((font-spec (format "%s-%d" +font-default +font-size)))
    (set-frame-font font-spec)
    (set-face-attribute 'default nil :font font-spec)
    (add-to-list 'default-frame-alist `(font . ,font-spec)))
  (set-face-attribute 'variable-pitch nil :font +font-variable-pitch)
  (set-face-attribute 'fixed-pitch nil :font +font-fixed-pitch)
  (setf (alist-get +font-variable-pitch face-font-rescale-alist 1.3 nil 'string=) 1.3))

(defun +load-ext-font ()
  (dolist (charset '(kana han hangul cjk-misc bopomofo symbol))
    (set-fontset-font
     (frame-parameter nil 'font)
     charset
     (font-spec :family +font-unicode)))
  ;; Specify font for Chinese characters
  (set-fontset-font t '(#x4e00 . #x9fff) (format "%s-%d" +font-cn 18)))

(defun +load-font ()
  (+load-base-font)
  (+load-ext-font))

(set-frame-parameter nil 'internal-border-width 10)
(setq-default left-margin-width 0 right-margin-width 2)
(set-window-margins nil 0 0)

;; auto change theme after system appearance changed
(defvar +theme-system-appearance nil
  "Weather to auto change theme after system appearance changed.")
(defvar +theme-system-light 'spacemacs-light
  "Theme used after change system appearance to light.")
(defvar +theme-system-dark 'spacemacs-dark
  "Theme used after change system appearance to dark.")
(when (and (boundp 'ns-system-appearance) (display-graphic-p) +theme-system-appearance)
  (add-to-list 'ns-system-appearance-change-functions
               (lambda (l?d)
                 (if (eq l?d 'light)
                     (load-theme +theme-system-light t)
                   (load-theme +theme-system-dark t)))))

(if (display-graphic-p)
    (progn
      (load-theme +theme t)
      (add-hook 'after-init-hook (lambda ()
                                   (+load-font)
                                   (global-ligature-mode))))
  (load-theme +theme-tui t))

(provide 'init-theme)
