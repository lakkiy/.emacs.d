;;; Bootstrap straight.el

;; https://www.reddit.com/r/emacs/comments/mtb05k/emacs_init_time_decreased_65_after_i_realized_the/
(setq straight-check-for-modifications '(check-on-save find-when-checking))
(setq straight-vc-git-default-clone-depth 1)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

;; require all packages in emacsclient
(setq eat-all-packages-daemon t)

;;; others
(let ((file-name-handler-alist nil))
  (require 'eat-package)
  (require 'init-eat)
  (require 'init-default)
  (require 'init-utils)
  (require 'init-builtin)
  (require 'init-shell)
  (require 'init-ui)
  (require 'init-dog)
  (require 'init-edit)
  (require 'init-completion)
  (require 'init-dev)
  (require 'init-windows)
  (when (and eat/enable-icon (display-graphic-p))
    (require 'init-icons))
  (require 'init-app)
  (require 'init-mode)
  (require 'init-org)
  (require 'init-lib)
  (require 'init-mole)
  (require 'init-xterm))
