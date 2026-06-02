;;; init-text.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;;; image
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

;;; markdown
(install-package 'markdown-mode)
(install-package 'markdown-indent-mode)
(install-package 'markdown-table-wrap)
(install-package 'grip-mode)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-hook 'markdown-mode-hook #'markdown-indent-mode)
(add-hook 'markdown-mode-hook #'valign-mode)

(defun my-wrap-table-at-point ()
  "Wrap the pipe table at point to fit the window."
  (interactive)
  (save-excursion
    (let* ((beg (progn (re-search-backward "^|" nil t)
                       (line-beginning-position)))
           (end (progn (re-search-forward "^[^|]" nil t)
                       (line-beginning-position)))
           (text (buffer-substring-no-properties beg (1- end)))
           (wrapped (markdown-table-wrap
                     text (window-width)
                     nil                     ; max cell height
                     markdown-hide-markup))) ; t when markup hidden
      (unless (equal wrapped text)
        (delete-region beg (1- end))
        (goto-char beg)
        (insert wrapped)))))

;;; LaTeX
(defun my/latex-mode-setup ()
  (when (executable-find "digestif")
    (company-mode 1)
    (eglot-ensure)))
(add-hook 'latex-mode-hook 'my/latex-mode-setup)

;;; typst
(install-package 'typst-ts-mode "https://git.sr.ht/~meow_king/typst-ts-mode")

;;; Ispell
;;
;; In Emacs 30 and newer, disable Ispell completion to avoid annotation errors
;; when no `ispell' dictionary is set.
(setq text-mode-ispell-word-completion nil)

(setq ispell-silently-savep t)

;;; Outline
(setq outline-minor-mode-cycle t
      outline-minor-mode-highlight t)

;;; init-text.el ends here
