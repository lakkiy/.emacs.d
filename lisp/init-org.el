;;; -*- lexical-binding: t -*-

(eat-package org-modern
  :straight (org-modern :type git :host github :repo "minad/org-modern")
  :hook
  (org-mode-hook . org-modern-mode)
  (org-agenda-finalize-hook . org-modern-agenda)
  :config
  (setq org-modern-star ["›"]
        ;; Use valign instead
        org-modern-table nil))

(eat-package org
  :straight (org :type built-in)
  :hook (org-mode-hook . eat/org-hook)
  :init
  (setq org-directory "~/Dropbox/org")
  (defvar load-language-list '((emacs-lisp . t)
                               (python . t)
                               (js . t)
                               (C . t)
                               (shell . t)))
  :config
  (setq org-edit-src-content-indentation 0
        org-src-fontify-natively nil
        org-confirm-babel-evaluate nil
        org-image-actual-width '(300)
        ;; Faster loading
        org-modules nil
        org-src-window-setup 'current-window
        org-log-done t)

  (require 'org-tempo) ;; see `org-structure-template-alist'
  (require 'ob)
  (require 'ob-dot)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (org-babel-do-load-languages 'org-babel-load-languages load-language-list))

(eat-package org-capture
  :init
  (global-set-key (kbd "C-c c") 'org-capture)

  (defun eat/org-capture-inbox ()
    (interactive)
    (org-capture nil "i"))
  (global-set-key (kbd "C-c i") #'eat/org-capture-inbox)

  (setq
   org-default-notes-file (concat org-directory "/default-notes.org")
   org-capture-templates
   `(("i" "Inbox" entry (file "~/Dropbox/org/inbox.org")
      "* TODO %?\n:PROPERITIES:\n:Created: %T\n:END:")
     ("w" "Work" entry (file+olp+datetree "~/Dropbox/org/Work.org")
      "* %^{Title}\n:PROPERITIES:\n:Created: %T\n:END:" :tree-type week)
     ("n" "Note" entry (file "~/Dropbox/org/Notes.org")
      "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t))))

(eat-package org-agenda
  :init
  (setq org-agenda-files (list org-directory)
        org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                                   (todo   . " ")
                                   (tags   . " %i %-12:c")
                                   (search . " %i %-12:c"))
        ;; hide any tag
        org-agenda-hide-tags-regexp ".")
  (global-set-key (kbd "C-c a") 'org-agenda)
  :config
  (setq org-agenda-current-time-string
        "⭠ now ─────────────────────────────────────────────────"))

(eat-package ox-gfm
  :straight t
  :config
  (add-to-list 'org-export-backends 'md))

(eat-package ob-restclient
  :straight t
  :init (cl-pushnew '(restclient . t) load-language-list)
  :config
  (add-to-list 'org-structure-template-alist '("rc" . "src restclient")))

(eat-package ob-go
  :straight t
  :init (cl-pushnew '(go .t) load-language-list)
  :config
  (add-to-list 'org-structure-template-alist '("go" . "src go")))

(eat-package restclient
  :straight t
  :mode ("\\.rest\\'" . restclient-mode)
  :init
  (defun eat/restclient ()
    "Work with `rest' in the *restclient* buffer."
    (interactive)
    (with-current-buffer (get-buffer-create "*restclient*")
      (restclient-mode)
      (pop-to-buffer (current-buffer)))))

(eat-package valign
  :straight t
  :init
  (setq valign-fancy-bar t)
  (when (display-graphic-p)
    (add-hook 'org-mode-hook #'valign-mode)))

(eat-package toc-org
  :straight t
  :commands toc-org-enable toc-org-insert-toc)

;;; Writing

(eat-package iimg
  :commands iimg-enable
  :hook ((text-mode-hook org-mode-hook) . iimg-enable))

(eat-package bklink
  :commands bklink-minor-mode
  :config
  (define-key bklink-minor-mode-map (kbd "C-c l") #'bklink-show-back-link)
  (define-key bklink-minor-mode-map (kbd "C-c i") #'bklink-insert))

(eat-package flique)

;; TODO search in pinyin
(eat-package xeft
  :straight (xeft :type git
                  :host github
                  :repo "casouri/xeft"
                  :files (:defaults "Makefile" "module"))
  :init
  (setq xeft-directory "~/Dropbox/org/roam"
        xeft-database "~/.xeft/db")
  :config
  (require 'flique)
  (defun xeft-setup ()
    (flique-append-to-index (buffer-file-name))
    (local-set-key (kbd "M-]") #'flique-forward)
    (local-set-key (kbd "M-[") #'flique-backward)
    (flique-show-navigation))
  (add-hook 'xeft-find-file-hook #'xeft-setup)
  (add-hook 'xeft-find-file-hook #'bklink-minor-mode))

(defvar eat/prose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-a") #'beginning-of-visual-line)
    (define-key map (kbd "C-e") #'end-of-visual-line)
    map)
  "Mode map for ‘eat/prose-mode’.")

(define-minor-mode eat/prose-mode
  "A mode that optimizes for prose editing."
  :lighter " PROSE"
  :keymap eat/prose-mode-map
  (if eat/prose-mode
      (progn
        (variable-pitch-mode 1)
        (visual-fill-column-mode 1)
        (setq-local cursor-type 'bar)
        (setq-local line-spacing 0.15)
        (corfu-mode -1)
        (setq-local whitespace-style '(tab-mark))
        (whitespace-mode))
    (visual-fill-column-mode -1)
    (whitespace-mode -1)
    (variable-pitch-mode -1)
    (kill-local-variable 'line-spacing)
    (kill-local-variable 'cursor-type)))

(defun eat/org-hook ()
  "Configuration for Org Mode."
  (electric-pair-local-mode -1)
  (electric-quote-local-mode)
  (electric-indent-local-mode -1))

(eat-package org-static-blog
  :straight t
  :init
  (setq org-static-blog-publish-title "404cn's blog")
  (setq org-static-blog-publish-url "https://404cn.github.io/")
  (setq org-static-blog-publish-directory "~/p/blog/")
  (setq org-static-blog-posts-directory "~/p/blog/posts/")
  (setq org-static-blog-drafts-directory "~/p/blog/drafts/")
  (setq org-static-blog-enable-tags t)
  (setq org-static-blog-use-preview t)
  (setq org-static-blog-preview-ellipsis "")
  (setq org-export-with-toc nil)
  (setq org-export-with-section-numbers nil)
  :config
  (setq org-static-blog-page-header (get-string-from-file "~/p/blog/static/header.html"))
  (setq org-static-blog-page-preamble (get-string-from-file "~/p/blog/static/preamble.html"))
  (setq org-static-blog-page-postamble (get-string-from-file "~/p/blog/static/postamble.html")))

;;; init-org.el ends here
(provide 'init-org)
