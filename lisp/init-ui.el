;;; init-ui.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;; form-feed
;;
;; =page-break-lines= 在开启 =bklink= 和 =visual-fill-column= 的 org buffer 中在 ^L 上移动会卡死，但是 form-feed 用着没有问题。
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

;;; init-ui.el ends here
