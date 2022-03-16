;;; -*- lexical-binding: t -*-
;; Do not use `eat-package' with themes.

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

;; `base16-theme'
(straight-use-package 'base16-theme)
(setq base16-distinct-fringe-background nil)

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

;; font
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
    (set-face-attribute 'fixed-pitch-serif frame :font fixed-pitch-font-spec))
  (custom-set-faces
   '(mode-line ((t :inherit variable-pitch)))
   '(mode-line-inactive ((t :inherit variable-pitch)))))

(defun +load-ext-font ()
  (when window-system
    (let ((font (frame-parameter nil 'font))
          (font-spec (font-spec :family +font-unicode)))
      (dolist (charset '(kana han hangul cjk-misc bopomofo symbol))
        (set-fontset-font font charset font-spec))))
  (setf (alist-get +font-unicode face-font-rescale-alist 0.7 nil 'string=) 0.7))

(defun +load-font ()
  (+load-base-font)
  (+load-face-font)
  (+load-ext-font))

;; load font and theme after theme created
(add-hook 'server-after-make-frame-hook
          (lambda ()
            (+load-font)
            (if (and (boundp 'ns-system-appearance)
                     +theme-system-appearance)
                (add-to-list 'ns-system-appearance-change-functions
                             (lambda (l?d)
                               (if (eq l?d 'light)
                                   (load-theme +theme-system-light t)
                                 (load-theme +theme-system-dark t))))
              (load-theme +theme t))))

;; TODO
;; spc a id agenda, not m-x
;; daemon start on tui, bui if you create a frame, it's gui, also `windows-system' become to non nil from nil
;; so config about `window-system' should update to check if emacs start with daemon (like `meow-indicator')

(provide 'init-ui)
