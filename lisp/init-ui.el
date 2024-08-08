;;; -*- lexical-binding: t -*-

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
  (if (display-graphic-p)
      (load-theme my/theme t)
    (load-theme my/theme-tui t)))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'my/setup-theme)
  (add-hook 'after-init-hook #'my/setup-theme))

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
(add-hook 'dired-mode-hook #'hl-todo-mode)
(add-hook 'prog-mode-hook #'hl-todo-mode)
(add-hook 'conf-mode-hook #'hl-todo-mode)

;; default-text-scale
(install-package 'default-text-scale)
(keymap-global-set "C-x C-=" #'default-text-scale-increase)
(keymap-global-set "C-x C--" #'default-text-scale-decrease)

;; breadcrumb
(install-package 'breadcrumb)
(setq-default frame-title-format
              '((:eval (breadcrumb-project-crumbs))
                (:eval (and imenu--index-alist
                            (concat "  ◊  " (breadcrumb-imenu-crumbs))))))

;;; init-ui.el ends here
