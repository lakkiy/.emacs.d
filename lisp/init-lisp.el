;;; init-lisp.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(setq-default lexical-binding t)

(install-package 'paredit)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'paredit-mode)
(add-hook 'scheme-mode-hook #'paredit-mode)
(add-hook 'lisp-mode-hook #'paredit-mode)

(install-package 'aggressive-indent)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'lisp-interaction-mode-hook #'aggressive-indent-mode)
(add-hook 'scheme-mode-hook #'aggressive-indent-mode)
(add-hook 'lisp-mode-hook #'aggressive-indent-mode)

;;; init-lisp.el ends here
