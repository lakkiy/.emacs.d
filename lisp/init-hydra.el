;;; -*- lexical-binding: t -*-

(eat-package pretty-hydra
  :straight t
  :init
  (+pdump-packages 'pretty-hydra)
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
      ("r" repeat-mode "repeat" :toggle t)
      ("f" follow-mode "follow" :toggle t)
      ("l" solaire-global-mode "solaire" :toggle t))
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
     ;; FIXME
     "Theme"
     (("t s" (load-theme 'spacemacs-light t) "space-light" :toggle t)
      ("t S" (load-theme 'spacemacs-dark t) "space-dark" :toggle t)
      ("t j" (load-theme 'joker t) "joker" :toggle t)
      ("t p" (load-theme 'printed t) "printed" :toggle t)
      ("t b" (load-theme 'storybook t) "storybook" :toggle t)
      ("t d" (load-theme 'minidark t) "minidark" :toggle t)
      ("t m" (load-theme 'modus-vivendi t) "modus-dark" :toggle t)
      ("t M" (load-theme 'modus-operandi t) "modus-light" :toggle t)
      ("t l" (load-theme 'lazycat-dark t) "lazycat-dark" :toggle t)
      ("t L" (load-theme 'lazycat-light t) "lazycat-light" :toggle t)
      ("t o" (load-theme 'atom-one-dark t) "one dark" :toggle t)
      ("t D" (load-theme 'dracula t) "dracula" :toggle t)
      ("t N" (load-theme 'nano t) "nano" :toggle t)
      ("t n" (load-theme 'nasy t) "nasy" :toggle t))))

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
  (global-set-key (kbd "<f6>") 'toggles-hydra/body))

;; (straight-use-package 'pretty-hydra)
;;
;; (+pdump-packages 'pretty-hydra)

;;; pretty-hydra
;; (pretty-hydra-define toggles-hydra (:title "Toggles" :quit-key "q")
;;       ("Basic"
;;        (("n" (if (fboundp 'display-line-numbers-mode)
;;                  (display-line-numbers-mode (if display-line-numbers-mode -1 1))
;;                (global-linum-mode (if global-linum-mode -1 1)))
;;          "line number"
;;          :toggle (or (bound-and-true-p display-line-numbers-mode) global-linum-mode))
;;         ("e" electric-pair-mode "electric pair" :toggle t)
;;         ("c" flyspell-mode "spell check" :toggle t)
;;         ("s" prettify-symbols-mode "pretty symbol" :toggle t)
;;         ("b" display-battery-mode "battery" :toggle t)
;;         ("i" display-time-mode "time" :toggle t)
;;         ("r" repeat-mode "repeat" :toggle t)
;;         ("f" follow-mode "follow" :toggle t)
;;         ("l" solaire-global-mode "solaire" :toggle t))
;;        "Highlight"
;;        (("h l" global-hl-line-mode "line" :toggle t)
;;         ("h p" show-paren-mode "paren" :toggle t)
;;         ("h r" rainbow-mode "rainbow" :toggle t)
;;         ("h d" rainbow-delimiters-mode "delimiter" :toggle t)
;;         ("h t" global-hl-todo-mode "todo" :toggle t))
;;        "Program"
;;        (("F" flymake-mode "flymake" :toggle t)
;;         ("O" hs-minor-mode "hideshow" :toggle t)
;;         ("u" subword-mode "subword" :toggle t)
;;         ("W" which-function-mode "which function" :toggle t)
;;         ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
;;         ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit))
;;         ("v" global-diff-hl-mode "gutter" :toggle t)
;;         ("V" diff-hl-flydiff-mode "live gutter" :toggle t)
;;         ("M" diff-hl-margin-mode "margin gutter" :toggle t)
;;         ("D" diff-hl-dired-mode "dired gutter" :toggle t))
;;        ;; FIXME
;;        "Theme"
;;        (("t s" (load-theme 'spacemacs-light t) "space-light" :toggle t)
;;         ("t S" (load-theme 'spacemacs-dark t) "space-dark" :toggle t)
;;         ("t j" (load-theme 'joker t) "joker" :toggle t)
;;         ("t p" (load-theme 'printed t) "printed" :toggle t)
;;         ("t b" (load-theme 'storybook t) "storybook" :toggle t)
;;         ("t d" (load-theme 'minidark t) "minidark" :toggle t)
;;         ("t m" (load-theme 'modus-vivendi t) "modus-dark" :toggle t)
;;         ("t M" (load-theme 'modus-operandi t) "modus-light" :toggle t)
;;         ("t l" (load-theme 'lazycat-dark t) "lazycat-dark" :toggle t)
;;         ("t L" (load-theme 'lazycat-light t) "lazycat-light" :toggle t)
;;         ("t o" (load-theme 'atom-one-dark t) "one dark" :toggle t)
;;         ("t D" (load-theme 'dracula t) "dracula" :toggle t)
;;         ("t N" (load-theme 'nano t) "nano" :toggle t)
;;         ("t n" (load-theme 'nasy t) "nasy" :toggle t))))

;; (pretty-hydra-define tab-bar (:title "Tab Bar" :quit-key "q")
;;   ("Options"
;;    (("n" tab-bar-new-tab "New")
;;     ("r" tab-rename "Rename")
;;     ("0" tab-bar-close-tab "Close current")
;;     ("1" tab-bar-close-other "Close other")
;;     ("u" tab-undo "Undo"))
;;    "Other Tab"
;;    (("b" switch-to-buffer-other-tab "Switch buffer")
;;     ("f" find-file-other-tab "Find file")
;;     ("d" dired-other-tab "Dired"))
;;    "Select"
;;    (("t" tab-bar-select-tab-by-name "Select by name")
;;     ("l" tab-next "Next Tab")
;;     ("h" tab-previous "Previous Tab"))))
;;
;; (global-set-key (kbd "C-x t") 'tab-bar/body)
;; (global-set-key (kbd "<f6>") 'toggles-hydra/body)

(provide 'init-hydra)
