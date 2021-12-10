;;; nano-theme-treemacs.el --- treemacs customization for nano theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:

;;; Code:
(unless (require 'all-the-icons nil t)
  (error "nano treemacs theme requires the all-the-icons package, like Atom."))

(defgroup nano-treemacs nil
  "Settings for nano's treemacs theme."
  :group 'nano-theme)

(with-eval-after-load 'treemacs
  (treemacs-create-theme
   "nano"
   :config
   (progn
     ;; TODO create icon
     ))
  (treemacs-load-theme "nano"))

(provide 'nano-theme-treemacs)
;;; nano-theme-treemacs.el ends here
