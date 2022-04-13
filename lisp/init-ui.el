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
(eat-package awesome-tray
  :straight (awesome-tray :type git :host github :repo "manateelazycat/awesome-tray")
  :hook (after-init-hook . (lambda ()
                             (require 'awesome-tray)
                             (awesome-tray-mode 1)))
  :init
  (setq
   awesome-tray-update-interval 0.5
   awesome-tray-minibuffer nil
   awesome-tray-essential-modules nil
   awesome-tray-info-padding-right 2 ;; or it will warp by meow
   ;; TODO  belong not work
   awesome-tray-active-modules '("buffer-read-only" "buffer-name" "mode-name" "belong" "location"))

  ;;Make the modeline in GUI a thin bar.
  (defface mini-modeline-mode-line
    '((((background light))
       :background "#55ced1" :height 0.14 :box nil)
      (t
       :background "#008b8b" :height 0.14 :box nil))
    "Modeline face for active window.")

  (defface mini-modeline-mode-line-inactive
    '((((background light))
       :background "#dddddd" :height 0.1 :box nil)
      (t
       :background "#333333" :height 0.1 :box nil))
    "Modeline face for inactive window.")

  :config
  (setq-default mode-line-format (when (display-graphic-p)
                                   '(" ")))

  ;; Do the same thing with opening buffers.
  (mapc
   (lambda (buf)
     (with-current-buffer buf
       (when (local-variable-p 'mode-line-format)
         (setq mode-line-format (when (display-graphic-p)
                                  '(" "))))
       ;; Make the modeline in GUI a thin bar.
       (when (and (local-variable-p 'face-remapping-alist)
                  (display-graphic-p))
         (setf (alist-get 'mode-line face-remapping-alist)
               'mini-modeline-mode-line
               (alist-get 'mode-line-inactive face-remapping-alist)
               'mini-modeline-mode-line-inactive))))
   (buffer-list))

  ;; Make the modeline in GUI a thin bar.
  (when (and (display-graphic-p))
    (let ((face-remaps (default-value 'face-remapping-alist)))
      (setf (alist-get 'mode-line face-remaps)
            'mini-modeline-mode-line
            (alist-get 'mode-line-inactive face-remaps)
            'mini-modeline-mode-line-inactive
            (default-value 'face-remapping-alist) face-remaps)))

  ;; disable popper in mode-line
  (with-eval-after-load 'popper
    (setq popper-mode-line " "))

  ;; rime
  (with-eval-after-load 'rime
    ;; TODO show in message have some error, wont on screen after choose
    (setq rime-show-candidate 'sidewindow)

    (add-to-list 'awesome-tray-module-alist
                 '("rime" . (rime-lighter)))
    (add-to-list 'awesome-tray-active-modules "rime"))

  ;; Add meow to awesome-tray
  (with-eval-after-load 'meow
    (defun awesome-tray-module-meow-info ()
      (string-trim (meow-indicator)))
    (add-to-list 'awesome-tray-module-alist
                 '("meow" . (awesome-tray-module-meow-info awesome-tray-module-evil-face)))
    (add-to-list 'awesome-tray-active-modules "meow")))

;;; init-ui.el ends here
(provide 'init-ui)
