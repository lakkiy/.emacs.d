;;; -*- lexical-binding: t -*-
;; Do not use `eat-package' with themes.

;;; Theme

;; disable previous theme when load theme
(defun +load-theme-advice (f theme-id &optional no-confirm no-enable &rest args)
  "Enhance `load-theme' by disabling other enabled themes & calling hooks"
  (unless no-enable ;
    (mapc #'disable-theme custom-enabled-themes))
  (prog1
      (apply f theme-id t no-enable args)
    (unless no-enable ;
      (pcase (assq theme-id +theme-hooks)
        (`(,_ . ,f) (funcall f))))))
(advice-add 'load-theme :around #'+load-theme-advice)

;; `tao-theme'
(straight-use-package 'tao-theme)

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

(defun my/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha-background) 100))
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha-background newalpha))))))

(global-set-key (kbd "M-C-8") (lambda () (interactive) (my/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (my/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha-background . 100)))))

;;; Packages

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
                                                  (+load-theme)
                                                  ;; enable awesome tray after theme loaded
                                                  (require 'awesome-tray)
                                                  (awesome-tray-mode 1)))

;;; Mode-line

(progn
  (eat-package mode-line-bell :straight t)
  (eat-package which-func :commands which-func-mode)
  (eat-package minions :straight t)

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

  (defun +setup-mode-line ()
    (mode-line-bell-mode)
    (which-func-mode)
    (minions-mode)
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
                      (:eval (when (fboundp 'meow-indicator) (meow-indicator)))
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
                      (:eval (when (bound-and-true-p flymake-mode) (sekiro-flymake-mode-line-format)))
                      ,spaces
                      (:eval (if (buffer-modified-p)
                                 ,(if (display-graphic-p) "ΦAΦ" "OAO")
                               ,(if (display-graphic-p) "ΦωΦ" "OwO")))
                      ,spaces
                      mode-line-misc-info
                      (:eval (concat (luna-mode-line-with-padding ,percentage)
                                     "%%"))
                      )))))

(add-hook 'after-init-hook #'+setup-mode-line)

;;; init-ui.el ends here
(provide 'init-ui)
