;;; -*- lexical-binding: t -*-

(eat-package ligature
  :straight (ligature :type git :host github :repo "mickeynp/ligature.el")
  :commands global-ligature-mode
  :hook (after-init-hook . (lambda () (global-ligature-mode t)))
  :config
  ;; https://htmlpreview.github.io/?https://github.com/kiliman/operator-mono-lig/blob/master/images/preview/normal/index.html
  (ligature-set-ligatures 'prog-mode
                          '("&&" "||" "|>" ":=" "==" "===" "==>" "=>"
                            "=<<" "!=" "!==" ">=" ">=>" ">>=" "->" "--"
                            "-->" "<|" "<=" "<==" "<=>" "<=<" "<!--" "<-"
                            "<->" "<--" "</" "+=" "++" "??" "/>" "__" "WWW")))

(defun +load-base-font ()
  (let ((font-spec (format "%s-%d" +font-default +font-size)))
    (set-frame-font font-spec)
    (set-face-attribute 'default nil :font font-spec)
    (add-to-list 'default-frame-alist `(font . ,font-spec)))
  (set-fontset-font t '(#x4e00 . #x9fff) +font-cn))

(defun +load-face-font (&optional frame)
  (let ((variable-pitch-font-spec (format "%s-%d" +font-variable-pitch +font-size))
        (fixed-pitch-font-spec (format "%s-%d" +font-default +font-size)))
    (set-face-attribute 'variable-pitch frame :font variable-pitch-font-spec)
    (set-face-attribute 'fixed-pitch frame :font fixed-pitch-font-spec)
    (set-face-attribute 'fixed-pitch-serif frame :font fixed-pitch-font-spec)))

(defun +load-ext-font ()
  (when window-system
    (let ((font (frame-parameter nil 'font))
          (font-spec (font-spec :family +font-unicode)))
      (dolist (charset '(kana han hangul cjk-misc bopomofo symbol))
        (set-fontset-font font charset font-spec))))
  (setf (alist-get +font-unicode face-font-rescale-alist 0.7 nil 'string=) 0.7)
  (setf (alist-get +font-variable-pitch face-font-rescale-alist 1.3 nil 'string=) 1.3))

(defun +load-font ()
  (+load-base-font)
  (+load-face-font)
  (+load-ext-font))

;; (set-frame-parameter nil 'internal-border-width 10)
;; (setq-default left-margin-width 0 right-margin-width 2)
;; (set-window-margins nil 0 0)

(add-hook 'after-init-hook
          (lambda ()
            (+load-font)))

(add-hook 'after-make-frame-functions
          (lambda (f)
            (+load-face-font f)
            (+load-ext-font)))

(provide 'init-font)
