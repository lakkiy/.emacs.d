(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push (expand-file-name "lisp" user-emacs-directory) load-path)
(push (expand-file-name "site-lisp" user-emacs-directory) load-path)

(require 'init-must)
(require 'init-utils)
(require 'init-font)

(when (eq system-type 'darwin)
  (require 'init-osx))

(icomplete-mode 1)
(icomplete-vertical-mode 1)
