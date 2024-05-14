;;; -*- lexical-binding: t -*-

;; image
(autoload #'iimg-enable "iimg")
(add-hook 'text-mode-hook #'iimg-enable)
(setq iimg-prune-slices-p nil)

;; TODO pngpaste and other system
;; TODO run screenshot and paste
(defun iimg-insert-clipboard (name)
  (interactive
   (list (let ((name (read-string "Caption/name for the image: ")))
           (if (equal name "")
               (format-time-string "%s")
             name))))
  (let ((image-file "/tmp/iimg.png"))
    (if (zerop (shell-command (concat "wl-paste -t image/png > " image-file)))
        (progn
          (iimg-insert image-file name t)
          (message "Image inserted successfully: %s" name)
          (delete-file image-file))
      (message "Failed to paste image from clipboard."))))

;; markdown
(install-package 'markdown-mode)
(install-package 'markdown-toc)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(setq markdown-enable-wiki-links t
      markdown-italic-underscore t
      markdown-asymmetric-header t
      markdown-make-gfm-checkboxes-buttons t
      markdown-gfm-uppercase-checkbox t
      markdown-fontify-code-blocks-natively t)

(defun my/markdown-mode-hook ()
  (when (executable-find "ltex-ls")
    (eglot-ensure))
  (when (display-graphic-p)
    (valign-mode 1)))
(add-hook 'markdown-mode-hook 'my/markdown-mode-hook)

;; LaTeX
(defun my/latex-mode-setup ()
  (when (executable-find "digestif")
    (company-mode 1)
    (eglot-ensure)))
(add-hook 'latex-mode-hook 'my/latex-mode-setup)

;; typst
(install-package 'typst-ts-mode "https://git.sr.ht/~meow_king/typst-ts-mode")

;;; init-text.el ends here
(provide 'init-text)
