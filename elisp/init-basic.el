;;; -*- lexical-binding: t -*-

(require 'init-funcs)

(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))

(when (display-graphic-p)
  ;; Set default font
  (cl-loop for font in '("SF Mono" "Hack" "Source Code Pro" "Fira Code"
                         "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas")
           when (font-installed-p font)
           return (set-face-attribute 'default nil
                                      :font font
                                      :height (cond (sys/mac-x-p 130)
                                                    (sys/win32p 110)
                                                    (t 100))))

  ;; Specify font for all unicode characters
  (cl-loop for font in '("Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Symbol")
           when (font-installed-p font)
           return(set-fontset-font t 'unicode font nil 'prepend))

  ;; Specify font for Chinese characters
  (cl-loop for font in '("WenQuanYi Micro Hei" "Microsoft Yahei")
           when (font-installed-p font)
           return (set-fontset-font t '(#x4e00 . #x9fff) font)))

(use-package elec-pair
  :straight (:type built-in)
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(use-package saveplace
  :straight (:type built-in)
  :hook (after-init . save-place-mode))

(use-package hideshow
  :straight (:type built-in)
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode))

(use-package so-long
  :straight (:type built-in)
  :config (global-so-long-mode 1))

(use-package paren
  :straight (:type built-in)
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

(use-package autorevert
  :straight (:type built-in)
  :ensure nil
  :hook (after-init . global-auto-revert-mode))

(use-package recentf
  :straight (:type built-in)
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers 'abbreviate-file-name))

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 10
      scroll-conservatively 100000)

(use-package which-key
  :init
  (setq which-key-idle-delay 1)
  (setq which-key-idle-secondary-delay 0.05)
  :config
  (which-key-mode))

(use-package auto-save
  :straight
  (auto-save :type git
             :host github
             :repo "manateelazycat/auto-save")
  :init
  (setq auto-save-silent t)
  (setq auto-save-disable-predicates
      '((lambda ()
      (string-suffix-p
      "go"
      (file-name-extension (buffer-name)) t))))
  :config
  (auto-save-enable))

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)))

(provide 'init-basic)
