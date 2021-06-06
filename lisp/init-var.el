;;; -*- lexical-binding: t -*-

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defvar +font
  (cl-loop for font in '("Operator Mono SSm Lig" "SF Mono" "Monaco" "Sarasa Mono SC" "Consolas"
                         "DejaVu Sans Mono")
           when (font-installed-p font)
           return font))

(defvar +font-cn
  (cl-loop for font in '("WenQuanYi Micro Hei" "Microsoft Yahei")
           when (font-installed-p font)
           return font))

(defvar +font-unicode
  (cl-loop for font in '("Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Symbol")
           when (font-installed-p font)
           return font))

(defvar +font-variable-pitch
  (cl-loop for font in '("Bookerly" "Overpass" "Verdana" "Lucida Grande")
           when (font-installed-p font)
           return font))

(defvar +font-height (cond (sys/macp 130)
                           (t 110)))

(defvar +use-header-line nil)

(defvar +theme 'doom-Iosvkem)

(defvar +enable-proxy? nil)

(defvar +proxy "127.0.0.1:7890")

(provide 'init-var)
