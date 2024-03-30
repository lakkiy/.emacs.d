;;; -*- lexical-binding: t -*-

(defvar my/theme 'pale
  "Default theme.")

(defvar after-load-theme-hook nil
  "Hooks run after `load-theme'.")

(defun my/load-theme (f theme &optional no-confirm no-enable &rest args)
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
(advice-add 'load-theme :around #'my/load-theme)

(add-hook 'after-init-hook #'(lambda ()
                               (when (display-graphic-p)
                                 (load-theme my/theme))))

(install-package 'almost-mono-themes)
(install-package 'standard-themes)
(install-package 'kaolin-themes)
(install-package 'spacemacs-theme)

(provide 'init-theme)
