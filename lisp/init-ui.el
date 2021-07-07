;;; -*- lexical-binding: t -*-
;; modeline and font

(eat-package ligature
  :straight (ligature :type git :host github :repo "mickeynp/ligature.el"))
(eat-package less-theme
  :straight (less-theme :type git :host github :repo "nobiot/less-theme"))

(eat-package nyan-mode :straight t)
(eat-package solaire-mode :straight t)

(eat-package rainbow-mode
  :straight t
  :commands rainbow-mode)

(eat-package nano-theme
  :init
  (setq nano-theme-light/dark 'light
        nano-theme-comment-italic nil
        nano-theme-keyword-italic nil
        nano-theme-system-appearance nil))

(eat-package emojify
  :straight t
  :command emojify-mode)

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

(eat-package modus-themes
  :straight t
  :init
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs t
        modus-themes-syntax 'green-strings
        modus-themes-no-mixed-fonts t
        modus-themes-paren-match 'intense-bold))

;; Nice window divider
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?┃))

(defun +format-mode-line ()
  (let* ((lhs '((:eval (meow-indicator))
                " "
                (:eval (rime-lighter))
                (:eval (propertize (+smart-file-name-cached) 'face 'bold))
                " Row %l Col %C"))
         (rhs '("%m"
                (vc-mode vc-mode)))
         (ww (window-width))
         (lhs-str (format-mode-line lhs))
         (rhs-str (format-mode-line rhs))
         (rhs-w (string-width rhs-str)))
    (format "%s%s%s"
            lhs-str
            (propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) (+ 1 ,rhs-w)))))
            rhs-str)))

(setq-default header-line-format nil)

;; Init or reload functions
(defun +init-ui (&optional frame)
  (when (display-graphic-p)
    (load-theme +theme t))
  ;; modeline
  (if +use-header-line
      (setq-default
       mode-line-format nil
       header-line-format '(:eval (+format-mode-line)))
    (setq-default mode-line-format '(:eval (+format-mode-line))))
  ;; load font
  (when (display-graphic-p)
    (set-face-attribute 'default frame :font +font :height +font-height)
    (set-fontset-font t 'unicode +font-unicode nil 'prepend)
    (set-fontset-font t '(#x4e00 . #x9fff) +font-cn)
    (set-fontset-font t 'symbol (font-spec :family +font-unicode) frame 'prepend)
    (set-frame-font +font nil (if frame (list frame) t))
    (set-face-attribute 'variable-pitch frame :font +font-variable-pitch :height +font-height)
    (set-face-attribute 'fixed-pitch frame :font +font :height +font-height))
  ;;; ligature
  (when (display-graphic-p)

    (require 'ligature)
    (global-ligature-mode t)

    (with-eval-after-load "ligature"
      ;; https://htmlpreview.github.io/?https://github.com/kiliman/operator-mono-lig/blob/master/images/preview/normal/index.html
      (ligature-set-ligatures 'prog-mode
                              '("&&" "||" "|>" ":=" "==" "===" "==>" "=>"
                                "=<<" "!=" "!==" ">=" ">=>" ">>=" "->" "--"
                                "-->" "<|" "<=" "<==" "<=>" "<=<" "<!--" "<-"
                                "<->" "<--" "</" "+=" "++" "??" "/>" "__" "WWW")))))

(defun +reload-ui-in-daemon (frame)
  "Reload the modeline and font in an daemon frame."
  (with-selected-frame frame
    (+init-ui frame)))

;; Load the modeline and fonts
(if (daemonp)
    (add-hook 'after-make-frame-functions #'+reload-ui-in-daemon)
  (+init-ui))

(provide 'init-ui)
