;;; -*- lexical-binding: t -*-

(defvar my/fonts-default
  '("Latin Modern Mono"
    "Roboto Mono"
    "Monaco"
    "Cascadia Code"
    "Menlo"
    "Source Code Pro")
  "List of fonts to try when setting the default font.")

(defvar my/fonts-variable-pitch
  '("Bookerly"
    "Cardo"
    "Times New Roman"
    "DejaVu Sans")
  "List of fonts to try when setting the variable-pitch font.")

(defvar my/fonts-cjk
  '("LXGW WenKai"
    "WenQuanYi Micro Hei"
    "Microsoft Yahei")
  "List of fonts to try when setting the CJK font.")

(defvar my/fonts-unicode '("Symbola")
  "List of fonts to try when setting the Unicode font.")

(defvar my/fonts-emoji
  '("Apple Color Emoji"
    "Segoe UI Symbol"
    "Noto Color Emoji")
  "List of fonts to try when setting the Emoji font.")

(defvar my/font-size-default 17
  "Default font size.")

(defvar my/theme 'modus-operandi
  "The default theme.")

(defvar my/theme-tui 'carbon
  "The default theme for TUI.")

(defvar after-load-theme-hook nil
  "Hooks run after `load-theme'.")

;;; init-var.el ends here
