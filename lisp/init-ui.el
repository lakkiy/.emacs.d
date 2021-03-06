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
  ;; Set default font
  (cl-loop for font in '("Operator Mono Lig" "SF Mono"  "Fira Code"
                          "DejaVu Sans Mono" "Consolas")
           when (font-installed-p font)
           return (set-face-attribute 'default nil
                                      :font font
                                      :height (cond (sys/mac-x-p 130)
                                                    (sys/win32p 110)
                                                    (t 110))))

  ;; Specify font for all unicode characters
  (cl-loop for font in '("Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Symbol")
           when (font-installed-p font)
           return(set-fontset-font t 'unicode font nil 'prepend))

  ;; Specify font for Chinese characters
  (cl-loop for font in '("WenQuanYi Micro Hei" "Microsoft Yahei")
           when (font-installed-p font)
           return (set-fontset-font t '(#x4e00 . #x9fff) font)))

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
