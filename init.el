(defvar +icons-p nil "Whether to enable `all-the-icons'.")

(defvar +font-default "Latin Modern Mono" "Default font.")
(defvar +font-size 15 "Default font size")
(defvar +font-unicode "Apple Color Emoji" "Emoji font.")
(defvar +font-cn "Sarasa Mono SC" "Just used for chinese font.")
(defvar +font-variable-pitch "Bookerly" "Used for `variable-pitch-mode'")

(defvar +theme 'leuven "Default theme.")
(defvar +theme-system-appearance t "Change theme on system appearance, only available on macOS.")
(defvar +theme-system-light 'leuven "Default light theme when `+theme-system-appearance' enabled.")
(defvar +theme-system-dark 'wombat "Default dark theme when `+theme-system-appearance' enabled.")
(defvar +theme-hooks nil "((theme-id . function) ...)")

(defvar +enable-benchmark nil "Run `benchmark-init/show-durations-tree'")
(defvar +meow-layout 'qwerty "Layout config for `meow', qwerty or dvorak")

;; Shut up!
(defun display-startup-echo-area-message() (message nil))

(let ((file-name-handler-alist nil))
  (require 'init-straight)
  (require 'init-basic)   ;; some basic config
  (require 'init-builtin) ;; builtin package config
  (require 'init-modeline)
  (require 'init-font) ;; TODO only config in GUI
  (require 'init-themes)
  (require 'init-meow)
  (require 'init-rime)
  (require 'init-minibuffer)
  (require 'init-window)
  (when (and +icons-p (display-graphic-p))
    (require 'init-icons))
  (require 'init-edit)
  (require 'init-completion)
  (require 'init-dev)
  (require 'init-telega)
  (require 'init-git)
  (require 'init-org)
  (require 'init-dired)
  (require 'init-ibuffer)
  (require 'init-spcfile)
  (require 'init-mole)
  (require 'init-mail)
  (unless window-system
    (require 'init-xterm))
  (add-hook 'after-init-hook 'server-mode)
  )
