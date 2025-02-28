;;; init-osx.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Have to enable menu bar on mac port, otherwise emacs lost focus.
(when (not (display-graphic-p))
  (menu-bar-mode 1))

(setq mac-option-modifier 'meta
      mac-command-modifier 'super)

;; https://github.com/d12frosted/homebrew-emacs-plus/issues/383
(setq insert-directory-program "gls")

;; `save-buffers-kill-emacs' will shutdown emacs daemon.
(global-set-key [(super q)] #'save-buffers-kill-terminal)
(global-set-key [(super a)] #'mark-whole-buffer)
(global-set-key [(super v)] #'yank)
(global-set-key [(super c)] #'kill-ring-save)
(global-set-key [(super s)] #'save-buffer)
(global-set-key [(super w)] #'delete-frame)
(global-set-key [(super z)] #'undo)

(setq ns-use-native-fullscreen nil
      ;; Render thinner fonts
      ns-use-thin-smoothing t
      ;; Don't open a file in a new frame
      ns-pop-up-frames nil)

;;; init-osx.el ends here
