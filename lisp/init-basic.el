;;; -*- lexical-binding: t -*-

(require 'init-utils)

(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))

(when (display-graphic-p)
  ;; Set default font
  (cl-loop for font in '("Operator Mono Lig" "SF Mono" "Hack" "Source Code Pro" "Fira Code"
                         "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas")
           when (font-installed-p font)
           return (set-face-attribute 'default nil
                                      :font font
                                      :height (cond (sys/mac-x-p 130)
                                                    (sys/win32p 110)
                                                    (t 110))))

  ;; Specify font for all unicode characters
  (cl-loop for font in '("Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Symbol")
           when (font-installed-p font)
           return(set-fontset-font t 'unicode font nil 'prepend))

  ;; Specify font for Chinese characters
  (cl-loop for font in '("WenQuanYi Micro Hei" "Microsoft Yahei")
           when (font-installed-p font)
           return (set-fontset-font t '(#x4e00 . #x9fff) font)))

(leaf elec-pair
  :tag "builtin"
  :hook (after-init-hook . electric-pair-mode)
  :init
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(leaf saveplace :tag "builtin" :hook (after-init-hook . save-place-mode))
(leaf hideshow :tag "builtin" :hook (prog-mode-hook . hs-minor-mode))
(leaf autorevert :tag "builtin" :hook (after-init-hook . global-auto-revert-mode))
(leaf so-long :global-minor-mode :tag "builtin" (global-so-long-mode 1))

(leaf paren
  :tag "builtin"
  :hook (after-init-hook . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil))
(setq scroll-step 3
      scroll-margin 10
      scroll-conservatively 100000)

(leaf which-key
  :straight t
  :init
  (setq which-key-idle-delay 1)
  (setq which-key-idle-secondary-delay 0.05)
  :global-minor-mode t)

(leaf auto-save
  :straight
  (auto-save :type git
             :host github
             :repo "manateelazycat/auto-save")
  :require t
  :init
  (setq auto-save-silent t)
  :custom
  (auto-save-idle . 3)
  :config
  (auto-save-enable))

(leaf exec-path-from-shell
  :straight t
  :when (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(provide 'init-basic)
