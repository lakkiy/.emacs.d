;;; -*- lexical-binding: t -*-
;;
;; NOETS: for subtree, press i in dired.

;; icon
(install-package 'nerd-icons-dired)

(defun my/dired-enable-icon ()
  (unless (file-remote-p default-directory)
    (nerd-icons-dired-mode 1)))

(add-hook 'dired-mode-hook #'my/dired-enable-icon)

;; sidebar
;;
;; file tree
(install-package 'dired-sidebar)
(setq dired-sidebar-theme 'ascii)

;; rsyncc
(install-package 'dired-rsync)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c C-r") 'dired-rsync))

;;; init-dired ends here
(provide 'init-dired)
