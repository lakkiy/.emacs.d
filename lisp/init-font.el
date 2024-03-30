;;; -*- lexical-binding: t -*-

(defvar my/fonts-default        '("Monaco" "Cascadia Code" "Menlo" "Source Code Pro"))
(defvar my/fonts-variable-pitch '("Bookerly" "Cardo" "Times New Roman" "DejaVu Sans"))
(defvar my/fonts-cjk            '("LXGW WenKai" "WenQuanYi Micro Hei" "Microsoft Yahei"))
(defvar my/fonts-unicode        '("Symbola"))
(defvar my/fonts-emoji          '("Apple Color Emoji" "Segoe UI Symbol" "Noto Color Emoji"))
(defvar my/font-size-default   13)

(defun font-installed-p (font-list)
  (let ((font-installed nil))
    (catch 'foo
      (dolist (font font-list)
        (when (find-font (font-spec :name font))
          (setq font-installed font)
          (throw 'foo t))))
    font-installed))

(defvar my/font-default        (font-installed-p my/fonts-default))
(defvar my/font-variable-pitch (font-installed-p my/fonts-variable-pitch))
(defvar my/font-cjk            (font-installed-p my/fonts-cjk))
(defvar my/font-unicode        (font-installed-p my/fonts-unicode))
(defvar my/font-emoji          (font-installed-p my/fonts-emoji))

(defun my/setup-font ()
  (set-face-attribute 'default nil :height (* 10 my/font-size-default))
  (when my/font-default
    (set-face-attribute 'default     nil :family my/font-default)
    (set-face-attribute 'fixed-pitch nil :font my/font-default))
  (when my/font-variable-pitch
    (set-face-font 'variable-pitch my/font-variable-pitch))
  (when my/font-unicode
    (set-fontset-font t 'unicode my/font-unicode))
  (when my/font-emoji
    (set-fontset-font t 'emoji   my/font-emoji))
  (when my/font-cjk
    (set-fontset-font t 'kana     my/font-cjk)
    (set-fontset-font t 'han      my/font-cjk)
    (set-fontset-font t 'cjk-misc my/font-cjk)))
(add-hook 'after-init-hook #'my/setup-font)

(defvar my/font-rescale-alist
  `((,my/font-cjk     . 0.95)
    (,my/font-emoji   . 0.9)
    (,my/font-unicode . 0.95))
  "A list of font names that should be rescaled.")

(defun my/rescale-font ()
  (interactive)
  (dolist (setting my/font-rescale-alist)
    (when (car setting)
      (setf (alist-get (car setting)
                       face-font-rescale-alist nil nil #'equal)
            (cdr setting)))))
(add-hook 'after-init-hook #'my/rescale-font)

(defun my/fixed-pitch-setup ()
  (interactive)
  (setq buffer-face-mode-face '(:family "Sarasa Mono SC"))
  (buffer-face-mode +1))

(provide 'init-font)
