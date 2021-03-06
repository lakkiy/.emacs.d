;;; -*- lexical-binding: t -*-
;; modeline and font

(require 'init-utils)
(require 'init-const)

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

;; FIXME font not work in daemon
(when (display-graphic-p)
  (set-face-attribute 'default nil :font *font* :height *font-height*)
  (set-fontset-font t 'unicode *font-unicode* nil 'prepend)
  (set-fontset-font t '(#x4e00 . #x9fff) *font-cn*))

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
