;;; -*- lexical-binding: t -*-

(leaf tab-bar
  :doc "save window layout, gui not work on macos"
  :tag "builtin"
  :when (eq system-type 'gnu/linux)
  :commands
  (tab-bar-mode tab-bar-new-tab tab-bar-rename-tab)
  :bind (("C-c M-t t" . tab-bar-mode)
         ("C-c M-t r" . tab-bar-rename-tab)
         ("C-c M-t n" . tab-bar-new-tab)
         ("C-c M-t d" . tab-bar-close-tab))
  :custom-face
  (tab-bar . '((t (:inherit mode-line))))
  (tab-bar-tab . '((t (:inherit mode-line))))
  (tab-bar-tab-inactive . '((t (:inherit mode-line-inactive))))
  :custom
  (tab-bar-show . 1)
  (tab-bar-new-tab-choice . "*scratch*")
  (tab-bar-close-button-show . nil)
  (tab-bar-new-button-show . nil))

(leaf yascroll
  :doc "yet another scroll bar"
  :straight t
  :hook
  ((prog-mode-hook . yascroll-bar-mode)
   (conf-mode-hook . yascroll-bar-mode)))

(leaf ace-window
  :commands
  (ace-swap-window ace-window)
  :custom-face
  (aw-leading-char-face . '((t (:inherit font-lock-keyword-face :bold t :height 3.0))))
  (aw-minibuffer-leading-char-face . '((t (:inherit font-lock-keyword-face :bold t :height 2.0))))
  (aw-mode-line-face . '((t (:inherit mode-line-emphasis :bold t))))
  :custom
  (aw-keys . '(?a ?o ?e ?u ?i))
  (aw-scope . 'frame))

(provide 'init-window)
