;;; -*- lexical-binding: t -*-

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'conf-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'subword-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-init-hook (lambda () (blink-cursor-mode -1)))
(fset 'yes-or-no-p 'y-or-n-p)

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

(leaf saveplace :tag "builtin" :doc "save latest edit place" :hook (after-init-hook . save-place-mode))
(leaf hideshow :tag "builtin" :doc "flod code" :hook (prog-mode-hook . hs-minor-mode))
(leaf autorevert :tag "builtin" :hook (after-init-hook . global-auto-revert-mode))
(leaf so-long :tag "builtin" :config (global-so-long-mode 1))
(leaf winner-mode :tag "builtin" :hook (after-init-hook . winner-mode))

(leaf elec-pair
  :tag "builtin"
  :hook (after-init-hook . electric-pair-mode)
  :init
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(leaf paren
  :tag "builtin"
  :doc "highlight paren"
  :hook (after-init-hook . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

(leaf which-key
  :straight t
  :init
  (setq which-key-idle-delay 1)
  (setq which-key-idle-secondary-delay 0.05)
  :global-minor-mode t)

(leaf auto-save
  :straight
  (auto-save :type git
             :host github
             :repo "manateelazycat/auto-save")
  :require t
  :init
  (setq auto-save-silent t)
  :custom
  (auto-save-idle . 3)
  :config
  (auto-save-enable))

(leaf exec-path-from-shell
  :straight t
  :when (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(provide 'init-basic)
