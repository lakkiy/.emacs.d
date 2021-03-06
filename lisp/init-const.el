;; -*- lexical-binding: t; -*-

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst *font*
  (cl-loop for font in '("Operator Mono Lig" "SF Mono"  "Fira Code"
                          "DejaVu Sans Mono" "Consolas")
           when (font-installed-p font)
           return font)
  "Font used for gui")

(defconst *font-unicode*
  (cl-loop for font in '("Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Symbol")
           when (font-installed-p font)
           return font)
  "Font used for display unicode")

(defconst *font-cn*
  (cl-loop for font in '("WenQuanYi Micro Hei" "Microsoft Yahei")
           when (font-installed-p font)
           return font)
  "Font used for display chinese")

(defconst *font-height*
  (cond (sys/mac-x-p 130)
        (sys/win32p 110)
        (t 110))
  "Font height")

(provide 'init-const)
