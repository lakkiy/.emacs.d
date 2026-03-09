;;; init-web.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(dolist (lang '((tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
                (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
                (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
                (css . ("https://github.com/tree-sitter/tree-sitter-css"))
                (svelte . ("https://github.com/tree-sitter-grammars/tree-sitter-svelte"))
                (jsdoc . ("https://github.com/tree-sitter/tree-sitter-jsdoc.git" nil "src"))))
  (add-to-list 'treesit-language-source-alist lang))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(install-package 'web-mode)
(add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((web-mode :language-id "svelte") . ("svelteserver" "--stdio"))))


;;; init-web.el ends here
