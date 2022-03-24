(defvar +font-default "Menlo"
  "Default font.")
(defvar +icons-p nil
  "Whether to enable `all-the-icons'.")
(defvar +font-size 12
  "Default font size")
(defvar +font-unicode "Apple Color Emoji"
  "Emoji font.")
(defvar +font-cn "LXGW WenKai"
  "Just used for chinese font.")
(defvar +font-variable-pitch "Cardo"
  "Used for `variable-pitch-mode'")

(defvar +theme 'nano
  "Default theme.")
(defvar +theme-tui 'carbon
  "Default theme in terminal.")
(defvar +theme-system-light 'nano
  "Default light theme after system appearance changed.")
(defvar +theme-system-dark 'carbon
  "Default dark theme. after system appearance changed.")
(defvar +theme-hooks nil
  "((theme-id . function) ...)")

(defvar +enable-benchmark nil
  "Enable `benchmark-init', run `benchmark-init/show-durations-tree' to see result.")
(defvar +meow-layout 'dvorak
  "Layout config for `meow', qwerty or dvorak.")

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(let ((file-name-handler-alist nil))
  ;;  must work on emacs -Q
  (require 'init-straight)
  (require 'init-builtin)
  ;; font theme modeline
  (require 'init-ui)
  ;;  some funcs
  (require 'init-utils)
  ;;  must work on normal
  (require 'init-dog)
  (require 'init-edit)
  (require 'init-completion)
  (require 'init-dev)
  (require 'init-windows)
  ;;  鸡肋
  (when (and +icons-p (display-graphic-p))
    (require 'init-icons))
  ;;  emacs app，telega，magit
  (require 'init-app)
  ;;  modes
  (require 'init-mode)
  (require 'init-org)
  ;;  lib like all-the-icons
  (require 'init-lib)
  ;;  just straight
  (require 'init-mole)
  (require 'init-xterm))
