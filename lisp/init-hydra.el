;;; -*- lexical-binding: t -*-

(straight-use-package 'pretty-hydra)

;;; pretty-hydra
(defun +change-theme (theme)
  "Disable other themes and load new one"
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

;; FIXME
(pretty-hydra-define toggles-hydra (:title "Toggles" :quit-key "q")
      ("Basic"
       (("n" (if (fboundp 'display-line-numbers-mode)
                 (display-line-numbers-mode (if display-line-numbers-mode -1 1))
               (global-linum-mode (if global-linum-mode -1 1)))
         "line number"
         :toggle (or (bound-and-true-p display-line-numbers-mode) global-linum-mode))
        ("e" electric-pair-mode "electric pair" :toggle t)
        ("c" flyspell-mode "spell check" :toggle t)
        ("s" prettify-symbols-mode "pretty symbol" :toggle t)
        ("b" display-battery-mode "battery" :toggle t)
        ("i" display-time-mode "time" :toggle t)
        ("r" repeat-mode "repeat" :toggle t))
       "Highlight"
       (("h l" global-hl-line-mode "line" :toggle t)
        ("h p" show-paren-mode "paren" :toggle t)
        ("h r" rainbow-mode "rainbow" :toggle t)
        ("h d" rainbow-delimiters-mode "delimiter" :toggle t)
        ("h t" global-hl-todo-mode "todo" :toggle t))
       "Program"
       (("F" flymake-mode "flymake" :toggle t)
        ("O" hs-minor-mode "hideshow" :toggle t)
        ("u" subword-mode "subword" :toggle t)
        ("W" which-function-mode "which function" :toggle t)
        ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
        ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit))
        ("v" global-diff-hl-mode "gutter" :toggle t)
        ("V" diff-hl-flydiff-mode "live gutter" :toggle t)
        ("M" diff-hl-margin-mode "margin gutter" :toggle t)
        ("D" diff-hl-dired-mode "dired gutter" :toggle t))
       "Theme"
       (("t s" (+change-theme 'spacemacs-light) "space-light" :toggle t)
        ("t S" (+change-theme 'spacemacs-dark) "space-dark" :toggle t)
        ("t j" (+change-theme 'joker) "joker" :toggle t)
        ("t p" (+change-theme 'printed) "printed" :toggle t)
        ("t b" (+change-theme 'storybook) "storybook" :toggle t)
        ("t m" (+change-theme 'modus-vivendi) "modus-dark" :toggle t)
        ("t M" (+change-theme 'modus-operandi) "modus-light" :toggle t)
        ("t n" (+change-theme 'nasy) "nasy" :toggle t))))

(pretty-hydra-define tab-bar (:title "Tab Bar" :quit-key "q")
  ("Options"
   (("n" tab-bar-new-tab "New")
    ("r" tab-rename "Rename")
    ("0" tab-bar-close-tab "Close current")
    ("1" tab-bar-close-other "Close other")
    ("u" tab-undo "Undo"))
   "Other Tab"
   (("b" switch-to-buffer-other-tab "Switch buffer")
    ("f" find-file-other-tab "Find file")
    ("d" dired-other-tab "Dired"))
   "Select"
   (("t" tab-bar-select-tab-by-name "Select by name")
    ("l" tab-next "Next Tab")
    ("h" tab-previous "Previous Tab"))))

(global-set-key (kbd "C-x t") 'tab-bar/body)
(global-set-key (kbd "C-h C-h") 'toggles-hydra/body)

(provide 'init-hydra)
