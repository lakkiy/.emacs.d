;;; -*- lexical-binding: t -*-
;; modeline and font

(straight-use-package '(ligature :type git :host github :repo "mickeynp/ligature.el"))
(straight-use-package '(lazycat-theme :type git :host github :repo "404cn/lazycat-theme"))
(straight-use-package '(modus-theme   :type git :host github :repo "protesilaos/modus-themes"))
(straight-use-package 'spacemacs-theme)

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

(setq
 modus-themes-slanted-constructs t
 modus-themes-bold-constructs t
 modus-themes-syntax 'green-strings
 modus-themes-no-mixed-fonts t
 modus-themes-paren-match 'intense-bold)

;;; Use window divider
(window-divider-mode 1)
;;; Nice window divider
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?┃))

;;; No cursor blink
(add-hook 'after-init-hook (lambda () (blink-cursor-mode -1)))

;;; No fringe in minibuffer
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-window-fringes
             (minibuffer-window frame) 0 0 nil t)))

(require 'init-utils)
(require 'init-const)

;; Init or reload functions
(defun +init-ui (&optional frame)
  (load-theme footheme t)
  ;; modeline
  (setq-default mode-line-format
                '((:eval
                   (+simple-mode-line-render
                    ;; left
                    '((:eval (meow-indicator))
                      " %l:%C "
                      (:propertize (-3 "%p") face +modeline-dim-face))
                    ;; right
                    '((:eval (rime-lighter))
                      " "
                      (:propertize mode-name face font-lock-keyword-face)
                      " "
                      (:eval (+smart-file-name-with-propertize))
                      " ")))))
  ;; load font
  (when (display-graphic-p)
    (set-face-attribute 'default frame :font *font* :height *font-height*)
    (set-fontset-font t 'unicode *font-unicode* nil 'prepend)
    (set-fontset-font t '(#x4e00 . #x9fff) *font-cn*)
    (set-fontset-font t 'symbol (font-spec :family *font-unicode*) frame 'prepend)
    (set-frame-font *font* nil (if frame (list frame) t))
    (set-face-attribute 'fixed-pitch frame :font *font* :height *font-height*)))

(defun +reload-ui-in-daemon (frame)
  "Reload the modeline and font in an daemon frame."
  (with-selected-frame frame
    (+init-ui frame)))

;; Load the modeline and fonts
(if (daemonp)
    (add-hook 'after-make-frame-functions #'+reload-ui-in-daemon)
  (+init-ui))

;; ligature
(when window-system

  (require 'ligature)
  (global-ligature-mode t)

  (with-eval-after-load "ligature"
    ;; https://htmlpreview.github.io/?https://github.com/kiliman/operator-mono-lig/blob/master/images/preview/normal/index.html
    (ligature-set-ligatures 'prog-mode
                            '("&&" "||" "|>" ":=" "==" "===" "==>" "=>"
                              "=<<" "!=" "!==" ">=" ">=>" ">>=" "->" "--"
                              "-->" "<|" "<=" "<==" "<=>" "<=<" "<!--" "<-"
                              "<->" "<--" "</" "+=" "++" "??" "/>" "__" "WWW"))))

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
  (define-key help-mode-tool-bar-map [Next\ Topic] nil))

(provide 'init-ui)
