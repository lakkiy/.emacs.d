;;; -*- lexical-binding: t -*-

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

;;; init-ui.el ends here
