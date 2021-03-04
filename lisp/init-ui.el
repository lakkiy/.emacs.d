;;; -*- lexical-binding: t -*-

(require 'init-utils)

(defun +simple-mode-line-render (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
               (string-width (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

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
