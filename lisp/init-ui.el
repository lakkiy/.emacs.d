;;; init-ui.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Variables
;;
;; NOTE Custom these in before-init.el
(defvar my/fonts-default '("Cascadia Code" "Monaco" "Menlo" "Source Code Pro"))
(defvar my/fonts-variable-pitch '("Bookerly" "Cardo" "Times New Roman" "DejaVu Sans"))
(defvar my/fonts-cjk '("LXGW WenKai" "FZYouSong GBK" "WenQuanYi Micro Hei" "Microsoft Yahei"))
(defvar my/fonts-symbol '("Apple Symbols"))
(defvar my/fonts-emoji '("Apple Color Emoji" "Segoe UI Symbol" "Noto Color Emoji"))
(defvar my/font-size-default 14)

(defvar my/theme 'modus-operandi)
(defvar my/theme-dark 'modus-vivendi)
(defvar after-load-theme-hook nil)

(defun lakki.is/wombat-white-cursor ()
  (when (eq (car custom-enabled-themes) 'wombat)
    (set-cursor-color "white")))
(add-hook 'after-load-theme-hook #'lakki.is/wombat-white-cursor)

;;; Font & Theme
(defun font-installed-p (font-list)
  (catch 'font-found
    (dolist (font font-list)
      (when (find-font (font-spec :name font))
        (throw 'font-found font)))))

(defun my/setup-font ()
  (let* ((my/font-default        (font-installed-p my/fonts-default))
         (my/font-variable-pitch (font-installed-p my/fonts-variable-pitch))
         (my/font-cjk            (font-installed-p my/fonts-cjk))
         (my/font-symbol        (font-installed-p my/fonts-symbol))
         (my/font-emoji          (font-installed-p my/fonts-emoji))
         (my/font-rescale-alist  `((,my/font-cjk    . 0.95)
                                   (,my/font-emoji  . 0.9)
                                   (,my/font-symbol . 0.95)
                                   (,my/font-variable-pitch . 1.2))))
    (set-face-attribute 'default nil :height (* 10 my/font-size-default))
    (when my/font-default
      (set-face-attribute 'default     nil :family my/font-default)
      (set-face-attribute 'fixed-pitch nil :font my/font-default))
    (when my/font-variable-pitch
      (set-face-font 'variable-pitch my/font-variable-pitch))
    (when my/font-emoji
      (set-fontset-font t 'emoji   my/font-emoji))
    (when my/font-symbol
      (set-fontset-font t 'symbol my/font-symbol))
    (when my/font-cjk
      (set-fontset-font t 'kana     my/font-cjk)
      (set-fontset-font t 'han      my/font-cjk)
      (set-fontset-font t 'cjk-misc my/font-cjk))
    (dolist (setting my/font-rescale-alist)
      (when (car setting)
        (setf (alist-get (car setting)
                         face-font-rescale-alist nil nil #'equal)
              (cdr setting))))))

(defun my/load-theme (f theme &optional no-confirm no-enable &rest args)
  (interactive
   (list
    (intern (completing-read "Theme: "
                             (mapcar #'symbol-name
				                     (custom-available-themes))))))
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (if (featurep (intern (format "%s-theme" theme)))
      (enable-theme theme)
    (apply f theme t no-enable args))
  (run-hooks 'after-load-theme-hook))
(advice-add 'load-theme :around #'my/load-theme)

(defun my/setup-theme ()
  (load-theme my/theme t))

(if (daemonp)
    (progn
      (add-hook 'server-after-make-frame-hook #'my/setup-font)
      (add-hook 'server-after-make-frame-hook #'my/setup-theme))
  (add-hook 'after-init-hook #'my/setup-font)
  (add-hook 'after-init-hook #'my/setup-theme))

(defun my/fixed-pitch-setup ()
  (interactive)
  (setq buffer-face-mode-face '(:family "Sarasa Mono SC"))
  (buffer-face-mode +1))

;; Favorite theme
(install-package 'spacemacs-theme)

;; form-feed
;;
;; =page-break-lines= 在开启 =bklink= 和 =visual-fill-column= 的 org buffer 中在 ^L 上移动会卡死，但是 form-feed 用着没有问题。
;; form-feed is vendored in site-lisp/form-feed.el (on `load-path'); this
;; autoload resolves from there, so no install-package is needed.
(autoload 'form-feed-mode "form-feed" nil t)
(add-hook 'org-mode-hook 'form-feed-mode)
(add-hook 'emacs-lisp-mode-hook 'form-feed-mode)
(add-hook 'text-mode-hook 'form-feed-mode)
(add-hook 'special-mode-hook 'form-feed-mode)

;; hl-todo
(install-package 'hl-todo)

(add-hook 'after-init-hook #'global-hl-todo-mode)

;; default-text-scale
(install-package 'default-text-scale)
(keymap-global-set "C-x C-=" #'default-text-scale-increase)
(keymap-global-set "C-x C--" #'default-text-scale-decrease)

(install-package 'solaire-mode)
(add-hook 'after-init-hook #'solaire-global-mode)

;; minions
;;
;; hide mode line minor mode
;;
;; Emacs 启动流程
;; ├── before-init-hook 设置临时变量、调试信息
;; │
;; ├── 加载 init.el
;; │   └── (use-package xxx)
;; │
;; ├── after-init-hook 启动非 UI 的功能（如 server-mode）
;; │
;; ├── window-setup-hook 启动 UI 相关功能（如 minions、modeline）
;; │
;; └── emacs-startup-hook 最后阶段的初始化或统计任务
(install-package 'minions)
(add-hook 'emacs-startup-hook #'minions-mode)

;; auto-dark
;;
;; Not work on mac for now
;; https://github.com/LionyxML/auto-dark-emacs/issues/58
(install-package 'auto-dark)
(setq auto-dark-themes `((,my/theme-dark) (,my/theme)))
(add-hook 'after-init-hook #'auto-dark-mode)
(add-hook 'auto-dark-dark-mode-hook #'lakki.is/wombat-white-cursor)

;;; nerd-icons-grep
;;
;; This setting is a pre-requirement, so an icon can be displayed near each heading
(setq grep-use-headings t)
(install-package 'nerd-icons-grep)

;;; init-ui.el ends here
