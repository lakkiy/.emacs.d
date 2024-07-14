;;; -*- lexical-binding: t -*-

;; winum
(install-package 'winum)

(setq winum-scope 'visible)

(keymap-global-set "M-0" #'winum-select-window-0-or-10)
(keymap-global-set "M-1" #'winum-select-window-1)
(keymap-global-set "M-2" #'winum-select-window-2)
(keymap-global-set "M-3" #'winum-select-window-3)
(keymap-global-set "M-4" #'winum-select-window-4)
(keymap-global-set "M-5" #'winum-select-window-5)
(keymap-global-set "M-6" #'winum-select-window-6)
(keymap-global-set "M-7" #'winum-select-window-7)
(keymap-global-set "M-8" #'winum-select-window-8)
(keymap-global-set "M-9" #'winum-select-window-9)

(add-hook 'after-init-hook #'winum-mode)


;; project-x, save window layout by project
(install-package 'project-x "https://github.com/karthink/project-x")
(with-eval-after-load 'project
  (project-x-mode 1))


;; popper
(install-package 'popper)
(add-hook 'after-init-hook #'popper-mode)

(setq popper-reference-buffers
      '("\\*Messages\\*"
        "Output\\*$"
        "\\*Async Shell Command\\*"
        "\\*Compile-Log\\*"
        "\\*Completions\\*"
        "\\Agenda Commands\\*"
        help-mode
        compilation-mode
        ghelp-page-mode

        "^\\*eshell.*\\*$" eshell-mode  ;eshell as a popup
        "^\\*shell.*\\*$"  shell-mode   ;shell as a popup
        "^\\*term.*\\*$"   term-mode    ;term as a popup
        "^\\*eat.*\\*$" eat-mode))

(global-set-key (kbd "C-`") 'popper-toggle)
(global-set-key (kbd "M-`") 'popper-cycle)
(global-set-key (kbd "C-M-`") 'popper-toggle-type)

(with-eval-after-load 'popper
  (require 'popper-echo)
  (popper-echo-mode +1)

  (defun my-popper-fit-window-height (win)
    "Determine the height of popup window WIN by fitting it to the buffer's content."
    (fit-window-to-buffer
     win
     (floor (frame-height) 3)
     (floor (frame-height) 3)))
  (setq popper-window-height #'my-popper-fit-window-height))


;; ace-window
(install-package 'ace-window)
(keymap-global-set "M-o" 'ace-window)
(setq aw-keys '(?a ?o ?e ?u ?i))


;; golden-ratio
(install-package 'golden-ratio)
(define-key mode-specific-map "\\" 'golden-ratio)


(provide 'init-window)
;;; init-window.el ends here
