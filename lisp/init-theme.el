;;; -*- lexical-binding: t -*-

(defvar eat/theme 'pale
  "Default theme.")

(defvar after-load-theme-hook nil
  "Hooks run after `load-theme'.")

(defun eat/load-theme (f theme &optional no-confirm no-enable &rest args)
  (interactive
   (list
    (intern (completing-read "Theme: "
                             (mapcar #'symbol-name
				     (custom-available-themes))))))
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (if (featurep (intern (format "%s-theme" theme)))
      (enable-theme theme)
    (apply f theme t no-enable args))
  (run-hooks 'after-load-theme-hook))
(advice-add 'load-theme :around #'eat/load-theme)

(add-hook 'after-init-hook #'(lambda ()
                               (when (display-graphic-p)
                                 (load-theme eat/theme))))

(install-package 'almost-mono-themes)
(install-package 'standard-themes)
(install-package 'kaolin-themes)
(install-package 'spacemacs-theme)
(install-package 'nano-theme "https://github.com/mrunhap/nano-theme.el")
(install-package 'carbon-theme "https://github.com/DogLooksGood/carbon-theme")
(install-package 'paperlike-theme "https://github.com/DogLooksGood/paperlike-theme")

(defun my/setup-custom-theme-load-path ()
  (add-to-list 'custom-theme-load-path (expand-file-name "nano-theme" package-user-dir))
  (add-to-list 'custom-theme-load-path (expand-file-name "carbon-theme" package-user-dir))
  (add-to-list 'custom-theme-load-path (expand-file-name "paperlike-theme" package-user-dir)))
(add-hook 'after-init-hook #'my/setup-custom-theme-load-path)

(provide 'init-theme)
