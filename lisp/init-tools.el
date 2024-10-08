;;; -*- lexical-binding: t -*-

(autoload #'color-outline-mode "color-outline.el" nil t)
(add-hook 'prog-mode-hook #'color-outline-mode)

;; gcmh
(install-package 'gcmh)
(setq gcmh-high-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook #'gcmh-mode)

;; vundo
(install-package 'vundo)
(install-package 'undo-hl "https://github.com/casouri/undo-hl.git")
(add-hook 'prog-mode-hook #'undo-hl-mode)
(add-hook 'conf-mode-hook #'undo-hl-mode)
(keymap-global-set "C-z" #'vundo)

;; isearch-mb
(install-package 'isearch-mb)
(add-hook 'isearch-mode-hook #'isearch-mb-mode)

(define-advice isearch-mb--update-prompt (:around (fn &rest args) show-case-fold-info)
  "Show case fold info in the prompt."
  (cl-letf* ((isearch--describe-regexp-mode-orig
              (symbol-function 'isearch--describe-regexp-mode))
             ((symbol-function 'isearch--describe-regexp-mode)
              (lambda (regexp-function &optional space-before)
                (concat (if isearch-case-fold-search "[Case Fold] " "")
                        (funcall isearch--describe-regexp-mode-orig
                                 regexp-function space-before)))))
    (apply fn args)))

(with-eval-after-load "isearch-mb"
  (keymap-set isearch-mb-minibuffer-map "C-h C-h" #'my/isearch-menu)
  (keymap-set isearch-mb-minibuffer-map "C-c C-o" #'isearch-occur))

;; webjump
(keymap-global-set "C-x C-/" #'webjump)
(setq webjump-sites
      '(("Emacs Wiki" . [simple-query "www.emacswiki.org" "www.emacswiki.org/cgi-bin/wiki/" #1=""])
        ("Emacs China" . "emacs-china.org")
        ("Emacs Reddit" . "www.reddit.com/r/emacs/")
        ("Emacs News" . "sachachua.com/blog/category/emacs-news/")
        ("Github" . [simple-query "github.com" "github.com/search?q=" #1#])
        ("Google" . [simple-query "google.com" "google.com/search?q=" #1#])
        ("Kagi" . [simple-query "kagi.com" "kagi.com/search?q=" #1#])
        ("Youtube" . [simple-query "youtube.com" "youtube.com/results?search_query=" #1#])
        ("Google Groups" . [simple-query "groups.google.com" "groups.google.com/groups?q=" #1#])
        ("stackoverflow" . [simple-query "stackoverflow.com" "stackoverflow.com/search?q=" #1#])
        ("Wikipedia" . [simple-query "wikipedia.org" "wikipedia.org/wiki/" #1#])))

;; wgrep
(install-package 'wgrep)
(setq wgrep-change-readonly-file t)
(add-hook #'grep-setup-hook #'wgrep-setup)

;; rg
;;
;; It has a menu command `rg-menu', UI better than urgrep and deadgrep.
(install-package 'rg)
(keymap-set project-prefix-map "r" #'rg-project)

;; avy
(install-package 'avy)
(with-eval-after-load 'avy
  (setq avy-background t
        avy-style 'pre))

;; pastebin
(install-package 'webpaste)

(setq webpaste-paste-confirmation t
      webpaste-add-to-killring t
      webpaste-provider-priority '("paste.mozilla.org")
      webpaste-open-in-browser nil)

;; separedit
(install-package 'separedit)
(keymap-global-set "C-c '" #'separedit)

;; xeft
(install-package 'xeft)
(setq xeft-directory "~/Dropbox/org/roam")

;; bklink; create back link
(setq bklink-summary-read-only-p t
      bklink-prune-summary-p nil)

(defun my/xeft-setup ()
  (visual-fill-column-mode 1)
  (require 'bklink)
  (bklink-minor-mode 1))

(with-eval-after-load 'xeft
  (add-hook 'xeft-find-file-hook #'my/xeft-setup))
(with-eval-after-load 'bklink
  (keymap-set bklink-minor-mode-map "C-t i" #'bklink-insert))

;; d2
(install-package 'd2-mode)
(install-package 'ob-d2)
(add-to-list 'auto-mode-alist '("\\.d2" . d2-mode))

;; csv-mode
(install-package 'csv-mode)
(defun my/csv-mode-setup ()
  (setq-local auto-hscroll-mode t))
(add-hook 'csv-mode-hook #'my/csv-mode-setup)

;; atomic-chrome
;;
;; Edit browser text with emacs.
(install-package 'atomic-chrome)
(setq atomic-chrome-buffer-open-style 'frame)
(add-hook 'after-init-hook #'atomic-chrome-start-server)

;; k8s
(install-package 'kubed)

;; ghelp
(install-package 'ghelp "https://github.com/casouri/ghelp.git")
(autoload #'ghelp-describe "ghelp")
(keymap-global-set "C-h C-h" #'ghelp-describe)
(with-eval-after-load 'ghelp
  (keymap-global-set "C-h r" #'ghelp-resume))

(install-package 'verb)

(install-package 'jwt)

;;; init-tools.el ends here
