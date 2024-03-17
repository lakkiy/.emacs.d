;;; -*- lexical-binding: t -*-

(defvar eat/fonts-default        '("Monaco" "Cascadia Code" "Menlo" "Source Code Pro"))
(defvar eat/fonts-variable-pitch '("Bookerly" "Cardo" "Times New Roman" "DejaVu Sans"))
(defvar eat/fonts-cjk            '("LXGW WenKai" "WenQuanYi Micro Hei" "Microsoft Yahei"))
(defvar eat/fonts-unicode        '("Symbola"))
(defvar eat/fonts-emoji          '("Apple Color Emoji" "Segoe UI Symbol" "Noto Color Emoji"))
(defvar eat/font-size-default   13)

(defun font-installed-p (font-list)
  (let ((font-installed nil))
    (catch 'foo
      (dolist (font font-list)
        (when (find-font (font-spec :name font))
          (setq font-installed font)
          (throw 'foo t))))
    font-installed))

(defvar eat/font-default        (font-installed-p eat/fonts-default))
(defvar eat/font-variable-pitch (font-installed-p eat/fonts-variable-pitch))
(defvar eat/font-cjk            (font-installed-p eat/fonts-cjk))
(defvar eat/font-unicode        (font-installed-p eat/fonts-unicode))
(defvar eat/font-emoji          (font-installed-p eat/fonts-emoji))

(defun eat/setup-font ()
  (set-face-attribute 'default nil :height (* 10 eat/font-size-default))
  (when eat/font-default
    (set-face-attribute 'default     nil :family eat/font-default)
    (set-face-attribute 'fixed-pitch nil :font eat/font-default))
  (when eat/font-variable-pitch
    (set-face-font 'variable-pitch eat/font-variable-pitch))
  (when eat/font-unicode
    (set-fontset-font t 'unicode eat/font-unicode))
  (when eat/font-emoji
    (set-fontset-font t 'emoji   eat/font-emoji))
  (when eat/font-cjk
    (set-fontset-font t 'kana     eat/font-cjk)
    (set-fontset-font t 'han      eat/font-cjk)
    (set-fontset-font t 'cjk-misc eat/font-cjk)))
(add-hook 'after-init-hook #'eat/setup-font)

(defvar eat/font-rescale-alist
  `((,eat/font-cjk     . 0.95)
    (,eat/font-emoji   . 0.9)
    (,eat/font-unicode . 0.95))
  "A list of font names that should be rescaled.")

(defun eat/rescale-font ()
  (interactive)
  (dolist (setting eat/font-rescale-alist)
    (when (car setting)
      (setf (alist-get (car setting)
                       face-font-rescale-alist nil nil #'equal)
            (cdr setting)))))
(add-hook 'after-init-hook #'eat/rescale-font)

(defun eat/fixed-pitch-setup ()
  (interactive)
  (setq buffer-face-mode-face '(:family "Sarasa Mono SC"))
  (buffer-face-mode +1))

(provide 'init-font)
