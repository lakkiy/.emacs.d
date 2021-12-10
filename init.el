(defvar +icons-p nil "Whether to enable `all-the-icons'.")

(defvar +font-default "Latin Modern Mono" "Font used for default.")
(defvar +font-size 15 "Default font size")
(defvar +font-unicode "Apple Color Emoji" "Emoji font.")
(defvar +font-fixed-pitch "FZPingXianYaSongS-R-GB" "Just used for chinese font.")
(defvar +font-variable-pitch "Bookerly" "Used for `variable-pitch-mode'")

(defvar +theme 'modus-vivendi "Theme used in GUI.")
(defvar +theme-system-appearance nil "Weather to auto change theme after system appearance changed.")
(defvar +theme-system-light 'modus-operandi "Theme used after change system appearance to light.")
(defvar +theme-system-dark 'modus-vivendi "Theme used after change system appearance to dark.")
(defvar +theme-hooks nil "((theme-id . function) ...)")

(defvar +enable-benchmark nil "Where to enable `benchmark'.")

(let ((file-name-handler-alist nil))
  (require 'init-straight)
  (require 'init-basic)
  (require 'init-builtin)
  (require 'init-theme)
  (require 'init-meow)
  (require 'init-rime)
  (require 'init-minibuffer)
  (require 'init-modeline)
  (require 'init-window)
  (when (and +icons-p (display-graphic-p))
    (require 'init-icons))
  (run-with-idle-timer
   1 nil
   #'(lambda ()
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
       (require 'init-fun)
       (add-hook 'after-init-hook 'server-mode))))
