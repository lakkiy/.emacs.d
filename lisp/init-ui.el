;;; -*- lexical-binding: t -*-
;; modeline and font

(straight-use-package '(ligature :type git :host github :repo "mickeynp/ligature.el"))

(require 'init-utils)
(require 'init-const)

;; Init or reload functions
(defun +init-ui (&optional frame)
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
(define-key tool-bar-map [copy] nil)
(define-key tool-bar-map [cut] nil)
(define-key tool-bar-map [new-file] nil)
(define-key tool-bar-map [open-file] nil)
(define-key tool-bar-map [dired] nil)
(define-key tool-bar-map [save-buffer] nil)
(define-key tool-bar-map [undo] nil)
(define-key tool-bar-map [paste] nil)
(define-key tool-bar-map [isearch-forward] nil)

(provide 'init-ui)
