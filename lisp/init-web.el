;;; init-web.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(install-package 'web-mode)
(add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((web-mode :language-id "svelte") . ("svelteserver" "--stdio"))))


;;; init-web.el ends here
