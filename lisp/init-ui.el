;;; -*- lexical-binding: t -*-
;; Do not use `eat-package' with themes.

;;; Theme

;; disable previous theme when load theme
(defun +load-theme-advice (f theme-id &optional no-confirm no-enable &rest args)
  "Enhance `load-theme' by disabling other enabled themes & calling hooks"
  (unless no-enable ;
    (mapc #'disable-theme custom-enabled-themes))
  (prog1
      (apply f theme-id no-confirm no-enable args)
    (unless no-enable ;
      (pcase (assq theme-id +theme-hooks)
        (`(,_ . ,f) (funcall f))))))
(advice-add 'load-theme :around #'+load-theme-advice)

(defun +load-theme ()
  (if (boundp 'ns-system-appearance)
      (add-to-list 'ns-system-appearance-change-functions
                   (lambda (l?d)
                     (if (eq l?d 'light)
                         (load-theme +theme-system-light t)
                       (load-theme +theme-system-dark t))))
    (load-theme +theme t)))

;; `color-theme-sanityinc-tomorrow'
(straight-use-package 'color-theme-sanityinc-tomorrow)

;; `doom-themes'
(straight-use-package 'doom-themes)

(with-eval-after-load 'doom-themes
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; `spacemacs-theme'
(straight-use-package 'spacemacs-theme)

(setq
 spacemacs-theme-comment-italic t
 spacemacs-theme-keyword-italic t
 spacemacs-theme-org-agenda-height t
 spacemacs-theme-org-bold t
 spacemacs-theme-org-height t
 spacemacs-theme-org-highlight t
 spacemacs-theme-org-priority-bold t
 spacemacs-theme-org-bold t
 spacemacs-theme-underline-parens t)

;; `kaolin-themes'
(straight-use-package 'kaolin-themes)

(setq
 kaolin-themes-underline-wave nil
 kaolin-themes-modeline-border nil
 kaolin-themes-modeline-padded 4)

(with-eval-after-load 'kaolin-themes
  (with-eval-after-load 'treemacs
    (with-eval-after-load 'all-the-icons
      (kaolin-treemacs-theme))))

;; `stimmung-themes'
(straight-use-package 'stimmung-themes)

(setq
 stimmung-themes-light-highlight-color "cornsilk1"
 stimmung-themes-dark-highlight-color "#40382b")

;;; Font
(defun +load-base-font ()
  (let ((font-spec (format "%s-%d" +font-default +font-size)))
    (set-frame-font font-spec)
    (set-face-attribute 'default nil :font font-spec)
    (add-to-list 'default-frame-alist `(font . ,font-spec)))
  (set-fontset-font t '(#x4e00 . #x9fff) +font-cn))

(defun +load-face-font ()
  (set-face-attribute 'variable-pitch nil :font +font-variable-pitch :height 1.3)
  (set-face-attribute 'fixed-pitch nil :font +font-default)
  (set-face-attribute 'fixed-pitch-serif nil :font +font-default)
  ;; make mode line use variable font but use original height
  (custom-set-faces
   `(mode-line ((t (:family ,+font-variable-pitch))))
   `(mode-line-inactive ((t (:family ,+font-variable-pitch))))))

(defun +load-ext-font ()
  (let ((font (frame-parameter nil 'font))
        (font-spec (font-spec :family +font-unicode)))
    (dolist (charset '(kana han hangul cjk-misc bopomofo symbol))
      (set-fontset-font font charset font-spec)))
  (set-fontset-font t 'emoji (font-spec :family +font-unicode) nil 'prepend)
  (setf (alist-get +font-unicode face-font-rescale-alist 0.7 nil 'string=) 0.7))

(defun +load-font ()
  (+load-base-font)
  (+load-face-font)
  (+load-ext-font))

;;; Packages
(eat-package diredfl
  :straight t
  :hook (dired-mode-hook . diredfl-global-mode)
  :config
  (require 'dired-x))

(eat-package default-text-scale
  :straight t
  :init
  (global-set-key (kbd "C-x C-=") #'default-text-scale-increase)
  (global-set-key (kbd "C-x C--") #'default-text-scale-decrease))

;;; Title format

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;;; TUI: only load tui theme

(add-hook 'after-make-console-frame-hooks (lambda ()
                                            (when (fboundp 'menu-bar-mode)
                                              (menu-bar-mode -1))
                                            (load-theme +theme-tui t)))
;;; GUI frame: load font and theme

(add-hook 'after-make-window-system-frame-hooks (lambda ()
                                                  (+load-font)
                                                  (+load-theme)))

;;; Mode-line

(eat-package mode-line-bell
  :straight t
  :hook (after-init-hook . mode-line-bell-mode))

(eat-package which-func
  :commands which-func-mode
  :hook (after-init-hook . which-func-mode))

(eat-package minions
  :straight t
  :hook (after-init-hook . minions-mode))

(defun luna-mode-line-with-padding (text)
  "Return TEXT with padding on the left.
The padding pushes TEXT to the right edge of the mode-line."
  (if (display-graphic-p)
      (let* ((len (string-pixel-width text))
             (space-prop
              `(space :align-to (- (+ right right-margin) (,len))))
             (padding (propertize "-" 'display space-prop)))
        (concat padding text))
    (concat " " text)))

(defun luna-mode-line-coding-system ()
  "Display abnormal coding systems."
  (let ((coding (symbol-name buffer-file-coding-system)))
    (if (or (and (not (string-prefix-p "prefer-utf-8" coding))
                 (not (string-prefix-p "utf-8" coding))
                 (not (string-prefix-p "undecided" coding)))
            (string-suffix-p "dos" coding))
        (concat "  " coding)
      "")))

(setq-default mode-line-format
              (let* ((spaces
                      (propertize " " 'display '(space :width 1.5)))
                     (fringe (propertize
                              " " 'display '(space :width fringe)))
                     (percentage
                      '(format
                        "[%%l] %d%%"
                        (/ (* (window-end) 100.0) (point-max)))))
                `(,fringe
                  (:eval (when (fboundp 'rime-lighter) (rime-lighter)))
                  " "
                  (:eval (if (window-dedicated-p) "🚷" ""))
                  (:eval (if buffer-read-only "🔒" ""))
                  (:propertize "%[%b%]" face (:inherit mode-line-buffer-id :weight bold))
                  (:eval (luna-mode-line-coding-system))
                  ,spaces
                  ,(propertize " " 'display '(raise 0.3))
                  ,(if (featurep 'minions)
                       'minions-mode-line-modes
                     'mode-line-modes)
                  ,(propertize " " 'display '(raise -0.3))
                  (:eval (when (bound-and-true-p flymake-mode) flymake-mode-line-format))
                  ,spaces
                  (:eval (if (buffer-modified-p)
                             ,(if (display-graphic-p) "ΦAΦ" "OAO")
                           ,(if (display-graphic-p) "ΦωΦ" "OwO")))
                  ,spaces
                  mode-line-misc-info
                  (:eval (concat (luna-mode-line-with-padding ,percentage)
                                 "%%"))
                  ;; (:eval (concat ,spaces "(%l) " ,percentage "%%"))
                  )))

;;; TODO
;; spc a id agenda, not m-x
;; daemon start on tui, bui if you create a frame, it's gui, also `windows-system' become to non nil from nil

;;; init-ui.el ends here
(provide 'init-ui)
