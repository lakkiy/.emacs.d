;;; -*- lexical-binding: t -*-

(straight-use-package '(dired-hacks :type git :host github :repo "Fuco1/dired-hacks"))

(eat-package dired-filter
  :hook (dired-mode-hook . dired-filter-group-mode)
  :init
  (setq dired-filter-revert 'never
        dired-filter-group-saved-groups
        '(("default"
           ("Git"
            (directory . ".git")
            (file . ".gitignore"))
           ("Directory"
            (directory))
           ("PDF"
            (extension . "pdf"))
           ("LaTeX"
            (extension "tex" "bib"))
           ("Source"
            (extension "c" "cpp" "hs" "rb" "py" "r" "cs" "el" "lisp" "html" "js" "css"))
           ("Doc"
            (extension "md" "rst" "txt"))
           ("Org"
            (extension . "org"))
           ("Archives"
            (extension "zip" "rar" "gz" "bz2" "tar"))
           ("Images"
            (extension "jpg" "JPG" "webp" "png" "PNG" "jpeg" "JPEG" "bmp" "BMP" "TIFF" "tiff" "gif" "GIF")))))
  :config
  (define-key dired-filter-map (kbd "p") 'dired-filter-pop-all)
  (define-key dired-filter-map (kbd "/") 'dired-filter-mark-map))

(eat-package dired-collapse
  :hook (dired-mode-hook . dired-collapse-mode))

(eat-package diredfl
  :straight t
  :hook (dired-mode-hook . diredfl-mode))

(eat-package dirvish
  :straight t
  ;; :after dired
  ;; :config
  ;;  (dirvish-override-dired-mode)
  )

(provide 'init-dired)
