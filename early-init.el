(setq
 ;; Do not initialise the package manager.  This is done in `init.el'.
 package-enable-at-startup nil
 ;; Resizing the Emacs frame can be a terribly expensive part of changing the
 ;; font. By inhibiting this, we easily halve startup times with fonts that are
 ;; larger than the system default.
 frame-inhibit-implied-resize t
 ;; After startup `gcmh' will reset this.
 gc-cons-threshold most-positive-fixnum
 gc-cons-percentage 0.6)

;; Faster to disable these here (before they've been initialized)
(push '(scroll-bar-mode . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(alpha-background . 90) default-frame-alist)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(defvar +icons-p nil "Whether to enable `all-the-icons'.")

(defvar +font-default "Menlo" "Default font.")
(defvar +font-size 12 "Default font size")
(defvar +font-unicode "Apple Color Emoji" "Emoji font.")
(defvar +font-cn "LXGW WenKai" "Just used for chinese font.")
(defvar +font-variable-pitch "DejaVu Serif" "Used for `variable-pitch-mode'")

(defvar +theme 'kaolin-breeze "Default theme.")
(defvar +theme-system-appearance t "Change theme on system appearance, only available on macOS.")
(defvar +theme-system-light 'kaolin-breeze "Default light theme when `+theme-system-appearance' enabled.")
(defvar +theme-system-dark 'kaolin-aurora "Default dark theme when `+theme-system-appearance' enabled.")
(defvar +theme-hooks nil "((theme-id . function) ...)")

(defvar +enable-benchmark nil "Run `benchmark-init/show-durations-tree'")
(defvar +meow-layout 'dvorak "Layout config for `meow', qwerty or dvorak")
