;;; -*- lexical-binding: t -*-

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
(setq xeft-directory "~/Dropbox/org/roam"
      xeft-database "~/.xeft/db"
      xeft-default-extension  "org")

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

;; pdf-tools
(install-package 'pdf-tools)

(autoload #'pdf-view-mode "pdf-tools")
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

(setq-default pdf-view-display-size 'fit-page)
;; Enable hiDPI support, but at the cost of memory! See politza/pdf-tools#51
(setq pdf-view-use-scaling t
      pdf-view-use-imagemagick nil)

(with-eval-after-load "pdf-tools"
  (pdf-tools-install-noverify)
  (keymap-substitute pdf-view-mode-map #'scroll-up-command #'pdf-view-scroll-up-or-next-page)
  (keymap-substitute pdf-view-mode-map #'scroll-down-command #'pdf-view-scroll-down-or-previous-page))

;; k8s
(install-package 'kubel)

;; restclient
(install-package 'restclient)

(add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode))

(defun restclient-buffer ()
  "Work with `rest' in the *restclient* buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*restclient*")
    (restclient-mode)
    (pop-to-buffer (current-buffer))))

;; ghelp
(install-package 'ghelp "https://github.com/casouri/ghelp.git")
(autoload #'ghelp-describe "ghelp")
(keymap-global-set "C-h C-h" #'ghelp-describe)
(with-eval-after-load 'ghelp
  (keymap-global-set "C-h r" #'ghelp-resume))

;; gptel
;;
;; store gpt key in ~/.authinfo
;; machine api.openai.com login apikey password ****
;; or
;; machine api.openai-sb.com login apikey password ****
(install-package 'gptel)

(setq gptel-default-mode 'org-mode
      gptel-org-branching-context t)

(add-hook 'gptel-mode-hook #'visual-fill-column-mode)

(defun gptel-openai-sb ()
  (gptel-make-openai "ChatGPT-SB"
    :host "api.openai-sb.com"
    :key (retrieve-authinfo-key "api.openai-sb.com" "apikey")
    :stream t
    :models '("gpt-4")))

(with-eval-after-load 'gptel
  (when (auth-source-search :host "api.openai-sb.com" :user "apikey")
    (setq-default gptel-backend (gptel-openai-sb))))

;; outli
(install-package 'outli "https://github.com/jdtsmith/outli")
(add-hook 'prog-mode-hook #'(lambda () (unless (file-remote-p default-directory) (outli-mode 1))))

;; database
(install-package 'pg)
(install-package 'pgmacs "https://github.com/emarsden/pgmacs")

;;; init-tools.el ends here
(provide 'init-tools)
