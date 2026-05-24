(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

(load (expand-file-name "lisp/init-defaults.el" user-emacs-directory) nil t t)
(load (expand-file-name "lisp/init-editor.el" user-emacs-directory) nil t t)

(fido-vertical-mode 1)
