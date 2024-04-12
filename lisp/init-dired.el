;;; -*- lexical-binding: t -*-

;;; icon
(install-package 'nerd-icons-dired)

(defun my/dired-enable-icon ()
  (unless (file-remote-p default-directory)
    (nerd-icons-dired-mode 1)))

(add-hook 'dired-mode-hook #'my/dired-enable-icon)

;;; init-dired ends here
(provide 'init-dired)
