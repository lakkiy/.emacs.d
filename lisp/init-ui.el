;;; -*- lexical-binding: t -*-
;; modeline and font

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
                    '((:propertize mode-name face font-lock-keyword-face)
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

(leaf ligature
  :straight
  (ligature :type git
            :host github
            :repo "mickeynp/ligature.el")
  :config
  ;; https://htmlpreview.github.io/?https://github.com/kiliman/operator-mono-lig/blob/master/images/preview/normal/index.html
  ;; for operator mono lig
  (ligature-set-ligatures 'prog-mode
                          '("&&" "||" "|>" ":=" "==" "===" "==>" "=>"
                            "=<<" "!=" "!==" ">=" ">=>" ">>=" "->" "--"
                            "-->" "<|" "<=" "<==" "<=>" "<=<" "<!--" "<-"
                            "<->" "<--" "</" "+=" "++" "??" "/>" "__" "WWW"))
  :global-minor-mode
  (global-ligature-mode))

(provide 'init-ui)
