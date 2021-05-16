;; From centaur
;; Faster to disable these here (before they've been initialized)
(push '(fullscreen . maximized) default-frame-alist)
(push '(scroll-bar-mode . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
(when (not (featurep 'ns))
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(menu-bar-lines . 0) default-frame-alist))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(require 'init-straight)
(require 'init-defaults)
(require 'init-themes)
