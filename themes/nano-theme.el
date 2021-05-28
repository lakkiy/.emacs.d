(defgroup nano-theme nil
  "Options of nano theme."
  :group 'faces)

(defcustom nano-theme-light/dark 'light
  "Nano theme uses light theme or dark theme?"
  :group 'nano-theme
  :type 'symbol)

(defun nano-theme--light?dark (light dark)
  "Determine using the LIGHT or the DARK color of nano-theme."
  (if (eq nano-theme-light/dark 'light)
      light
    dark))
(defalias '--l?d #'nano-theme--light?dark)

(deftheme nano "Theme split from nano-emacs")

(let ((foreground (--l?d "#37474F" "#ECEFF4"))
      (background (--l?d "#FFFFFF" "#2E3440"))
      (highlight (--l?d "#FAFAFA" "#3B4252"))
      (critical (--l?d "#FF6F00" "#EBCB8B"))
      (salient (--l?d "#673AB7" "#81A1C1"))
      (strong (--l?d "#000000" "#ECEFF4"))
      (popout (--l?d "#FFAB91" "#D08770"))
      (subtle (--l?d "#ECEFF1" "#434C5E"))
      (faded (--l?d "#B0BEC5" "#677691")))
  (custom-theme-set-faces
   `nano
   `(default ((t (:foreground ,foreground :background ,background))))
   `(cursor                         ((t (:background ,foreground))))
   ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'nano)
