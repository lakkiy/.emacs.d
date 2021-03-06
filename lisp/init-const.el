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

(defconst *font-list*
  '("Operator Mono Lig" "SF Mono"  "Fira Code"
    "DejaVu Sans Mono" "Consolas")
  "Font list, the first installed font will set to default font")

(defconst *font-unicode-list*
  '("Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Symbol"))

(defconst *font-cn-list*
  '("WenQuanYi Micro Hei" "Microsoft Yahei"))

;; FIXME nil in daemon, maybe move to early init?
;; or don't use list
(defconst *font*
  (cl-loop for font in *font-list*
           when (font-installed-p font)
           return font)
  "Font used for gui")

(defconst *font-unicode*
  (cl-loop for font in *font-unicode-list*
           when (font-installed-p font)
           return font)
  "Font used for display unicode")

(defconst *font-cn*
  (cl-loop for font in *font-cn-list*
           when (font-installed-p font)
           return font)
  "Font used for display chinese")

(defconst *font-height*
  (cond (sys/mac-x-p 130)
        (sys/win32p 110)
        (t 110))
  "Font height")

(provide 'init-const)
