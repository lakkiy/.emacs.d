;;; -*- lexical-binding: t -*-

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'conf-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'subword-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq use-short-answers t)

;;; project.el use C-x p
(global-unset-key (kbd "C-x C-p"))
(global-set-key (kbd "C-x C-d") #'dired)

(defun +reopen-file-with-sudo ()
  (interactive)
  (find-alternate-file (format "/sudo::%s" (buffer-file-name))))

(global-set-key (kbd "C-x C-z") #'+reopen-file-with-sudo)
;; (global-set-key (kbd "<f7>") #'profiler-start)
;; (global-set-key (kbd "<f8>") #'profiler-report)

;; use mouse left click to find definitions
(global-unset-key (kbd "C-<down-mouse-1>"))
(global-set-key (kbd "C-<mouse-1>") #'xref-find-definitions-at-mouse)

(global-set-key (kbd "C-c M-t t") 'tab-bar-mode)
(global-set-key (kbd "C-c M-t r") 'tab-bar-rename-tab)
(global-set-key (kbd "C-c M-t n") 'tab-bar-new-tab)
(global-set-key (kbd "C-c M-t d") 'tab-bar-close-tab)

;; Vertical Scroll
(setq scroll-step 1)
(setq scroll-margin 15)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
;; Horizontal Scroll
(setq hscroll-step 1)
(setq hscroll-margin 10)
(setq ;; install hunspell and hunspell-en_US
      ispell-dictionary "en_US"
      ispell-program-name "hunspell"
      ispell-personal-dictionary (expand-file-name "hunspell_dict.txt" user-emacs-directory))

(add-hook 'after-init-hook 'save-place-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'after-init-hook 'global-auto-revert-mode)
(add-hook 'after-init-hook 'global-so-long-mode)
(add-hook 'after-init-hook 'winner-mode)

(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(add-hook 'after-init-hook 'electric-pair-mode)

(add-hook 'after-init-hook 'show-paren-mode)
(setq
 show-paren-when-point-in-periphery t
 show-paren-when-point-inside-paren t)

(straight-use-package 'which-key)
(straight-use-package 'exec-path-from-shell)
(straight-use-package '(auto-save :type git :host github :repo "manateelazycat/auto-save"))

;; which-key
(setq
 which-key-idle-delay 1
 which-key-idle-secondary-delay 0.05)

(add-hook 'after-init-hook 'which-key-mode)

;; auto-save
(setq auto-save-silent t
      auto-save-idle 3)

;; FIXME
(require 'auto-save)
(auto-save-enable)

;; exec-path-from-shell
(when (memq window-system '(mac ns x))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(provide 'init-basic)
