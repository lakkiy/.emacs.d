;;; init-tools.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(autoload #'color-outline-mode "color-outline.el" nil t)
(add-hook 'prog-mode-hook #'color-outline-mode)

;;; gcmh
(install-package 'gcmh)
(setq gcmh-high-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook #'gcmh-mode)

;;; vundo
(install-package 'vundo)
(install-package 'undo-hl "https://github.com/casouri/undo-hl.git")
(add-hook 'prog-mode-hook #'undo-hl-mode)
(add-hook 'conf-mode-hook #'undo-hl-mode)
(keymap-global-set "C-z" #'vundo)

;; isearch-mb
(install-package 'isearch-mb)
(add-hook 'isearch-mode-hook #'isearch-mb-mode)
(with-eval-after-load "isearch-mb"
  (keymap-set isearch-mb-minibuffer-map "C-c C-o" #'isearch-occur))

;;; grep
(install-package 'wgrep)
(setq wgrep-change-readonly-file t)
(add-hook #'grep-setup-hook #'wgrep-setup)

;; It has a menu command `rg-menu', UI better than urgrep and deadgrep.
(install-package 'rg)
(keymap-set project-prefix-map "r" #'rg-project)

;;; avy
(install-package 'avy)
(with-eval-after-load 'avy
  (setq avy-background t
        avy-style 'pre))

;;; pastebin
(install-package 'webpaste)

(setq webpaste-paste-confirmation t
      webpaste-add-to-killring t
      webpaste-open-in-browser nil)

;;; separedit
(install-package 'separedit)
(keymap-global-set "C-c '" #'separedit)

;;; xeft
;;
;; When build on m mac, use /opt/homebrew as prefix
;; tar xf xapian-core.tar.xz
;; cd xapian-core/
;; ./configure --prefix=/opt
;; make
;; make install
(install-package 'xeft)
(setq xeft-directory "~/Dropbox/org/roam"
      xeft-default-extension "org")

;; bklink; create back link
(setq bklink-summary-read-only-p t
      bklink-prune-summary-p nil)

;; TODO icon, marginalia like find-file, preview
(defun xeft-search ()
  (interactive)
  (unless (featurep 'xeft)
    (require 'xeft))
  (let* ((files (funcall xeft-file-list-function))
         (choices
          (mapcar (lambda (file)
                    (cons (with-temp-buffer
                            (insert-file-contents file nil 0 300)
                            (goto-char (point-min))
                            (let ((title (funcall xeft-title-function file)))
                              (if (and title (not (string-empty-p title)))
                                  title
                                (file-name-nondirectory file))))
                          file))
                  files))
         (choice (completing-read "Xeft: " choices nil nil)))
    (let* ((exist-cons (assoc choice choices))
           (file-path (if exist-cons
                          (cdr exist-cons)
                        (expand-file-name (funcall xeft-filename-fn choice) xeft-directory))))
      (unless (file-exists-p file-path)
        (when (y-or-n-p (format "Create file `%s'? " choice))
          (find-file file-path)
          (insert choice "\n\n")
          (save-buffer)
          (xeft--front-page-cache-refresh)
          (run-hooks 'xeft-find-file-hook)))
      (when (file-exists-p file-path)
        (find-file file-path)
        (run-hooks 'xeft-find-file-hook)))))

(defun my-xeft-init-org-note ()
  (interactive)
  "Auto-insert org header when creating a new xeft note if appropriate."
  (when (and (eq major-mode 'org-mode)
             (< (count-lines (point-min) (point-max)) 3))
    (save-excursion
      (goto-char (point-min))
      (let ((title (string-trim (thing-at-point 'line t))))
        ;; 只有当第一行不是 #+TITLE 才改，防止误伤已有笔记
        (unless (string-prefix-p "#+TITLE:" title)
          (erase-buffer)
          (insert (format "#+TITLE: %s\n\n* References\n" title))
          (save-buffer))))))

(defun my-xeft-setup ()
  (my-xeft-init-org-note)
  (require 'bklink)
  (bklink-minor-mode 1))

(with-eval-after-load 'xeft
  (add-hook 'xeft-find-file-hook #'my-xeft-setup))
(with-eval-after-load 'bklink
  (keymap-set bklink-minor-mode-map "C-t i" #'bklink-insert))

;;; d2
(install-package 'd2-mode)
(install-package 'ob-d2)
(add-to-list 'auto-mode-alist '("\\.d2" . d2-mode))

;;; atomic-chrome
;;
;; Edit browser text with emacs.
(install-package 'atomic-chrome)
(setq atomic-chrome-buffer-open-style 'frame)
(add-hook 'after-init-hook #'atomic-chrome-start-server)

;;; ghelp
(install-package 'ghelp "https://github.com/casouri/ghelp.git")
(autoload #'ghelp-describe "ghelp")
(keymap-global-set "C-h C-h" #'ghelp-describe)
(with-eval-after-load 'ghelp
  (keymap-global-set "C-h r" #'ghelp-resume))

;;; uniline
;;
;; https://emacs-china.org/t/unline-emacs-package/28112/5?u=rua
(install-package 'uniline)

(setq meow--kbd-forward-char "<right>"
      meow--kbd-backward-char "<left>"
      meow--kbd-forward-line "<down>"
      meow--kbd-backward-line "<up>")

(with-eval-after-load "uniline"
  (keymap-set uniline-mode-map "C-c /" 'uniline-hydra-choose-body)
  (keymap-set uniline-mode-map "C-c u" 'uniline--set-brush-0)
  (keymap-set uniline-mode-map "C-c -" 'uniline--set-brush-1)
  (keymap-set uniline-mode-map "C-c +" 'uniline--set-brush-2)
  (keymap-set uniline-mode-map "C-c =" 'uniline--set-brush-3)
  (keymap-set uniline-mode-map "C-c #" 'uniline--set-brush-block)
  (keymap-set uniline-mode-map "-" nil)
  (keymap-set uniline-mode-map "+" nil)
  (keymap-set uniline-mode-map "#" nil)
  (keymap-set uniline-mode-map "=" nil))


;;; misc
(install-package 'csv-mode)
(install-package 'kubed)
(install-package 'verb)
(install-package 'jwt)
(install-package 'sicp)
(install-package 'speed-type)
(install-package 'elcord)
(install-package 'dwim-shell-command)

;;; init-tools.el ends here
