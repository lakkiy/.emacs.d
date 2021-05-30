;;; -*- lexical-binding: t -*-
;; modeline and font

(straight-use-package '(ligature :type git :host github :repo "mickeynp/ligature.el"))
(straight-use-package '(modus-theme   :type git :host github :repo "protesilaos/modus-themes"))
(straight-use-package 'spacemacs-theme)
(straight-use-package 'atom-one-dark-theme)
(straight-use-package 'dracula-theme)
(straight-use-package 'gotham-theme)
(straight-use-package 'minimal-theme)
(straight-use-package 'tao-theme)
(straight-use-package 'doom-themes)
(straight-use-package 'emojify)
(straight-use-package 'solaire-mode)

;;; nano-theme
(setq
 nano-theme-comment-italic t
 nano-theme-keyword-italic t)

;;; doom-themes
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)

(defun +change-doom-theme (theme)
  "Change theme and enable solaire-global-mode"
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (if (bound-and-true-p solaire-global-mode)
      (solaire-global-mode -1))
  (solaire-global-mode +1)
  (load-theme theme t))

;;; solaire-mode
(add-hook 'after-init-hook (lambda ()
                             (solaire-global-mode 1)))

;;; emojify
(add-hook 'after-init-hook #'global-emojify-mode)

;;; spacemacs-theme
(setq
 spacemacs-theme-comment-italic t
 spacemacs-theme-keyword-italic t
 spacemacs-theme-org-agenda-height t
 spacemacs-theme-org-bold t
 spacemacs-theme-org-height t
 spacemacs-theme-org-highlight t
 spacemacs-theme-org-priority-bold t
 spacemacs-theme-org-bold t
 spacemacs-theme-underline-parens t)

;;; modus-theme
(setq
 modus-themes-slanted-constructs t
 modus-themes-bold-constructs t
 modus-themes-syntax 'green-strings
 modus-themes-no-mixed-fonts t
 modus-themes-paren-match 'intense-bold)

;; no cursor blink
;; (add-hook 'after-init-hook (lambda () (blink-cursor-mode -1)))

;; Nice window divider
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?┃))

;;; No fringe in minibuffer
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-window-fringes
             (minibuffer-window frame) 0 0 nil t)))

(require 'init-utils)
(require 'init-const)

(defun +format-mode-line ()
  (let* ((lhs '((:eval (meow-indicator))
                " "
                (:eval (rime-lighter))
                " "
                (:eval (+smart-file-name-cached))
                " Ln %l Col %C"))
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
  (load-theme +theme t)
  ;; modeline
  (if (eq system-type 'darwin)
      (setq-default mode-line-format '(:eval (+format-mode-line)))
    (setq-default
     mode-line-format nil
     header-line-format '(:eval (+format-mode-line))))
  ;; load font
  (when (display-graphic-p)
    (set-face-attribute 'default frame :font +font :height +font-height)
    (set-fontset-font t 'unicode +font-unicode nil 'prepend)
    (set-fontset-font t '(#x4e00 . #x9fff) +font-cn)
    (set-fontset-font t 'symbol (font-spec :family +font-unicode) frame 'prepend)
    (set-frame-font +font nil (if frame (list frame) t))
    (set-face-attribute 'fixed-pitch frame :font +font :height +font-height))
  ;;; ligature
  (when window-system

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

;;; tool-bar for mac
(when (eq system-type 'darwin)
  (define-key tool-bar-map [copy] nil)
  (define-key tool-bar-map [cut] nil)
  (define-key tool-bar-map [new-file] nil)
  (define-key tool-bar-map [open-file] nil)
  (define-key tool-bar-map [dired] nil)
  (define-key tool-bar-map [save-buffer] nil)
  (define-key tool-bar-map [undo] nil)
  (define-key tool-bar-map [paste] nil)
  (define-key tool-bar-map [isearch-forward] nil)
  (define-key help-mode-tool-bar-map [search] nil)
  (define-key help-mode-tool-bar-map [Previous\ Topic] nil)
  (define-key help-mode-tool-bar-map [Next\ Topic] nil)
  (setq
   info-tool-bar-map nil
   isearch-tool-bar-map nil)
  (with-eval-after-load "vc-dir"
    (setq vc-dir-tool-bar-map nil)))

(provide 'init-ui)
