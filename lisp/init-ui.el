;;; -*- lexical-binding: t -*-
;; Do not use `eat-package' with themes.

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

(when (and (boundp 'ns-system-appearance) (display-graphic-p) +theme-system-appearance)
  (add-to-list 'ns-system-appearance-change-functions
               (lambda (l?d)
                 (if (eq l?d 'light)
                     (load-theme +theme-system-light t)
                   (load-theme +theme-system-dark t)))))

;; `atom-one-dark-theme'
(straight-use-package 'atom-one-dark-theme)

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
  ;; NOTE maybe check `+icons-p' and `all-the-icons'
  (with-eval-after-load 'treemacs
    (with-eval-after-load 'all-the-icons
      (kaolin-treemacs-theme))))

;; `the-matrix-theme'
;; FIXME can see modeline text with telephone-line
(straight-use-package 'the-matrix-theme)

;; FIXME not work on emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (unless +theme-system-appearance
              (load-theme +theme t))))

(defun +load-base-font ()
  (let ((font-spec (format "%s-%d" +font-default +font-size)))
    (set-frame-font font-spec)
    (set-face-attribute 'default nil :font font-spec)
    (add-to-list 'default-frame-alist `(font . ,font-spec)))
  (set-fontset-font t '(#x4e00 . #x9fff) +font-cn))

(defun +load-face-font (&optional frame)
  (let ((variable-pitch-font-spec (format "%s-%d" +font-variable-pitch +font-size))
        (fixed-pitch-font-spec (format "%s-%d" +font-default +font-size)))
    (set-face-attribute 'variable-pitch frame :font variable-pitch-font-spec)
    (set-face-attribute 'fixed-pitch frame :font fixed-pitch-font-spec)
    (set-face-attribute 'fixed-pitch-serif frame :font fixed-pitch-font-spec)))

(defun +load-ext-font ()
  (when window-system
    (let ((font (frame-parameter nil 'font))
          (font-spec (font-spec :family +font-unicode)))
      (dolist (charset '(kana han hangul cjk-misc bopomofo symbol))
        (set-fontset-font font charset font-spec))))
  (setf (alist-get +font-unicode face-font-rescale-alist 0.7 nil 'string=) 0.7)
  (setf (alist-get +font-variable-pitch face-font-rescale-alist 1.3 nil 'string=) 1.3))

(defun +load-font ()
  (+load-base-font)
  (+load-face-font)
  (+load-ext-font))

;; (set-frame-parameter nil 'internal-border-width 10)
;; (setq-default left-margin-width 0 right-margin-width 2)
;; (set-window-margins nil 0 0)

(add-hook 'after-init-hook
          (lambda ()
            (+load-font)))

(add-hook 'after-make-frame-functions
          (lambda (f)
            (+load-face-font f)
            (+load-ext-font)))

;; Hide vc backend in modeline
(defadvice vc-mode-line (after strip-backend () activate)
  (when (stringp vc-mode)
    (let ((my-vc (replace-regexp-in-string "^ Git." "" vc-mode)))
      (setq vc-mode my-vc))))

;; TODO process icon in terminal and check +icons-p
(eat-package telephone-line
  :straight t
  :hook (after-init-hook . telephone-line-mode)
  :init
  (defvar modeline-height 17)
  ;; Set mode-line height
  (setq telephone-line-height modeline-height)

  (setq-default mode-line-format nil)
  (require 'telephone-segments)

  ;; Set separator style
  (setq telephone-line-primary-left-separator 'telephone-line-halfsin-left)
  (setq telephone-line-primary-right-separator 'telephone-line-halfsin-right)

  ;; Set subseparator
  ;; TODO: function to choose separator by name
  (when window-system
    (setq telephone-line-secondary-left-separator 'telephone-line-identity-hollow-left
          telephone-line-secondary-right-separator 'telephone-line-identity-hollow-right))

  ;; Left edge
  ;; meow project-buffer
  ;; TODO: gray background for buffer and mode segment in inactive line
  (setq telephone-line-lhs
        '((accent . ((my-meow-segment :active)))
          (nil . (my-project-buffer-segment))
          (nil . (my-modified-status-segment))
          (nil . (my-read-only-status-segment))
          ))
  ;; (nil    . (my-flycheck-segment))))

  ;; Right edge
  ;; vc pos major encoding
  (setq telephone-line-rhs
        '((nil    . (my-rime-segment))
          (nil    . (my-vc-segment))
          (accent . ((my-position-segment :active)))
          (nil    . ((my-major-mode-segment-icon :active)))
          (accent . ((my-coding-segment :active)))
          (nil    . (my-flymake-segment))
          ))

  ;; TODO rime flymake/flycheck anzu
  )

(provide 'init-ui)
