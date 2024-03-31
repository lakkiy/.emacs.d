;;; -*- lexical-binding: t -*-

(install-package 'almost-mono-themes)
(install-package 'standard-themes)
(install-package 'kaolin-themes)
(install-package 'spacemacs-theme)

;;; hl-line, disabled
(setq hl-line-sticky-flag nil)

;; (when (display-graphic-p)
;;   (add-hook 'prog-mode-hook #'hl-line-mode)
;;   (add-hook 'conf-mode-hook #'hl-line-mode))

(defun eat/hl-line-setup ()
  "Disable `hl-line-mode' if region is active."
  (when (and (bound-and-true-p hl-line-mode)
             (region-active-p))
    (hl-line-unhighlight)))

(with-eval-after-load 'hl-line
  (add-hook 'post-command-hook #'eat/hl-line-setup))

;;; form-feed
;;
;; =page-break-lines= 在开启 =bklink= 和 =visual-fill-column= 的 org buffer 中在 ^L 上移动会卡死，但是 form-feed 用着没有问题。
(autoload 'form-feed-mode "form-feed" nil t)
(add-hook 'org-mode-hook 'form-feed-mode)
(add-hook 'emacs-lisp-mode-hook 'form-feed-mode)
(add-hook 'text-mode-hook 'form-feed-mode)
(add-hook 'special-mode-hook 'form-feed-mode)

;;; minions
(install-package 'minions)
(add-hook 'after-init-hook 'minions-mode)

;;; hl-todo
(install-package 'hl-todo)
(add-hook 'dired-mode-hook #'hl-todo-mode)
(add-hook 'prog-mode-hook #'hl-todo-mode)
(add-hook 'conf-mode-hook #'hl-todo-mode)

;;; default-text-scale
(install-package 'default-text-scale)
(keymap-global-set "C-x C-=" #'default-text-scale-increase)
(keymap-global-set "C-x C--" #'default-text-scale-decrease)

;;; breadcrumb
(install-package 'breadcrumb)
(setq-default frame-title-format
              '((:eval (breadcrumb-project-crumbs))
                (:eval (and imenu--index-alist
                            (concat "  ◊  " (breadcrumb-imenu-crumbs))))))

(provide 'init-ui)
