;;; -*- lexical-binding: t -*-
;;
;; NOETS: for subtree, press i in dired.

;; icon
(install-package 'nerd-icons-dired)

(defun my/dired-enable-icon ()
  (unless (file-remote-p default-directory)
    (nerd-icons-dired-mode 1)))

(add-hook 'dired-mode-hook #'my/dired-enable-icon)

;; rsyncc
(install-package 'dired-rsync)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c C-r") 'dired-rsync))

(install-package 'treemacs)
(install-package 'treemacs-nerd-icons)
(install-package 'treemacs-magit)
(install-package 'treemacs-tab-bar)

(with-eval-after-load 'treemacs
  (require 'treemacs-magit)
  (require 'treemacs-tab-bar)
  (require 'treemacs-nerd-icons)
  (treemacs-load-theme "nerd-icons")
  (treemacs-set-scope-type 'Tabs))


;;; init-dired ends here
(provide 'init-dired)
