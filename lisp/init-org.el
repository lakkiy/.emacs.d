;;; -*- lexical-binding: t -*-

(eat-package org-modern
  :straight (org-modern :type git :host github :repo "minad/org-modern")
  :hook
  (org-mode-hook . org-modern-mode))

(eat-package org
  :straight (org :type built-in)
  :init
  ;; rescale image with for org-download
  ;; use #+attr_org :width 300px to rescale
  (setq org-image-actual-width nil)
  (setq org-directory "~/Dropbox/org")
  (setq org-highlight-latex-and-related '(latex))

  (setq
   org-startup-indented t ;; NOTE maybe conflict with org-modern, but don't have other choice
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t
   org-tags-column 0
   ;; Highlight latex text in org mode
   org-highlight-latex-and-related '(latex script entities)
   org-src-window-setup 'current-window
   org-log-done t
   org-html-checkbox-type 'unicode
   org-todo-keywords        (quote ((sequence "TODO(t)" "WIP(w/!)" "WAIT(W@/!)" "HOLD(h)" "|" "CANCELLED(c@/!)" "DONE(d!/!)")))
   org-todo-repeat-to-state "NEXT"
   org-todo-keyword-faces   (quote (("NEXT" :inherit warning)
  				                    ("WAIT" :inherit font-lock-string-face))))
  (defvar load-language-list '((emacs-lisp . t)
                               (perl . t)
                               (python . t)
                               (ruby . t)
                               (js . t)
                               (css . t)
                               (sass . t)
                               (C . t)
                               (java . t)
                               (shell . t)
                               (plantuml . t)))

  ;; `org-babel-load-languages' 在初始化的时候只存放 (LANG . nil)，表示需禁止的语言。
  ;; 其它所有需要的语言都动态加载，加载成功后存入 `org-babel-load-languages'
  (defun my/org-babel-execute-src-block (&optional _arg info _params)
    "Load language if needed"
    (let* ((lang (nth 0 info))
           (sym (if (member (downcase lang) '("c" "cpp" "c++")) 'C (intern lang)))
           (backup-languages org-babel-load-languages))
      ;; - (LANG . nil) 明确禁止的语言，不加载。
      ;; - (LANG . t) 已加载过的语言，不重复载。
      (unless (assoc sym backup-languages)
        (condition-case err
            (progn
              (org-babel-do-load-languages 'org-babel-load-languages (list (cons sym t)))
              (setq-default org-babel-load-languages (append (list (cons sym t)) backup-languages)))
          (file-missing
           (setq-default org-babel-load-languages backup-languages)
           err)))))
  (advice-add 'org-babel-execute-src-block :before #'my/org-babel-execute-src-block )
  :config
  (require 'org-tempo)
  (require 'ob)
  (require 'ob-dot)
  (org-babel-do-load-languages 'org-babel-load-languages load-language-list))

(eat-package org-capture
  :init
  (global-set-key (kbd "C-c c") 'org-capture)

  (defun get-year-and-month ()
    (list (format-time-string "%Y年") (format-time-string "%m月")))

  (defun find-month-tree ()
    (let* ((path (get-year-and-month))
           (level 1)
           end)
      (unless (derived-mode-p 'org-mode)
        (error "Target buffer \"%s\" should be in Org mode" (current-buffer)))
      (goto-char (point-min))
      (dolist (heading path)
        (let ((re (format org-complex-heading-regexp-format
                          (regexp-quote heading)))
              (cnt 0))
          (if (re-search-forward re end t)
              (goto-char (point-at-bol))
            (progn
              (or (bolp) (insert "\n"))
              (if (/= (point) (point-min)) (org-end-of-subtree t t))
              (insert (make-string level ?*) " " heading "\n"))))
        (setq level (1+ level))
        (setq end (save-excursion (org-end-of-subtree t t))))
      (org-end-of-subtree)))

  (setq
   org-default-notes-file (concat org-directory "/default-notes.org")
   org-capture-templates
   `(("b" "Billing" plain (file+function "~/Dropbox/org/Billing.org" (lambda () (find-month-tree)))
      " | %U | %^{Category} | %^{Description} | %^{Amount} |")
     ("w" "Work" entry (file+olp+datetree "~/Dropbox/org/Work.org")
      "* %^{Title}\n:PROPERITIES:\n:Created: %T\n:END:" :tree-type week)
     ("j" "Journal" entry (file+olp+datetree "~/Dropbox/org/Journal.org")
      "*  %^{Title} %?\n%U\n%a\n" :clock-in t :clock-resume t)
     ("o" "Book" entry (file+olp+datetree "~/Dropbox/org/Book.org")
	  "* Topic: %^{Description}  %^g %? Added: %U")
     ("n" "Note" entry (file "~/Dropbox/org/Notes.org")
      "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t))))

(eat-package org-agenda
  :init
  (setq org-agenda-files (list org-directory))
  (global-set-key (kbd "C-c a") 'org-agenda)
  :config
  (setq org-agenda-current-time-string
        "⭠ now ─────────────────────────────────────────────────")
  ;; https://200ok.ch/posts/2022-02-13_integrating_org_mode_agenda_into_other_calendar_apps.html
  ;; export agenda to iCalendar
  ;; Setting variables for the ics file path
  (setq org-agenda-private-local-path "/tmp/dummy.ics")
  ;; (setq org-agenda-private-remote-path "/sshx:user@host:path/dummy.ics")
  (setq org-agenda-private-remote-path "~/Sync/dummy.ics")
  ;; Define a custom command to save the org agenda to a file
  (setq org-agenda-custom-commands
        `(("X" agenda "" nil ,(list org-agenda-private-local-path)))))

(eat-package ox-gfm
  :straight t
  :config
  (add-to-list 'org-export-backends 'md))

(eat-package ob-restclient
  :straight t
  :init (cl-pushnew '(restclient . t) load-language-list))

(eat-package ob-go
  :straight t
  :init (cl-pushnew '(go .t) load-language-list))

(eat-package restclient
  :straight t
  :mode ("\\.rest\\'" . restclient-mode)
  :init
  (defun sanityinc/restclient ()
    "Work with `rest' in the *restclient* buffer."
    (interactive)
    (with-current-buffer (get-buffer-create "*restclient*")
      (restclient-mode)
      (pop-to-buffer (current-buffer)))))

(when (display-graphic-p)
  (eat-package valign
    :straight t
    :hook (org-mode-hook . valign-mode)
    :init
    (setq valign-fancy-bar t)))

(eat-package easy-hugo
  :straight t
  :commands easy-hugo
  :init
  (setq easy-hugo-server-flags "-D"
        easy-hugo-basedir "~/bookshelf/"
        easy-hugo-previewtime "300"
        easy-hugo-default-ext ".org"
        easy-hugo-org-header t))

(eat-package toc-org
  :straight t
  :commands toc-org-enable toc-org-insert-toc)

;;; Writing

(eat-package iimg
  :commands iimg-enable
  :hook (text-mode-hook . iimg-enable))

(eat-package bklink
  :commands bklink-minor-mode
  :config
  (define-key bklink-minor-mode-map (kbd "C-c l") #'bklink-show-back-link)
  (define-key bklink-minor-mode-map (kbd "C-c i") #'bklink-insert))

(eat-package flique)

;; TODO search in pinyin
(eat-package xeft
  :straight (xeft
             :type git :host github :repo "casouri/xeft"
             :pre-build ("make")
             :files (:defaults "Makefile" "*.h" "*.cc" "*.so"))
  :init
  (setq xeft-directory "~/Dropbox/org/roam"
        xeft-database "~/Dropbox/org/roam/db")
  :config
  (require 'flique)
  (defun xeft-setup ()
    (auto-fill-mode)
    (flique-append-to-index (buffer-file-name))
    (local-set-key (kbd "M-]") #'flique-forward)
    (local-set-key (kbd "M-[") #'flique-backward)
    (flique-show-navigation))
  (add-hook 'xeft-find-file-hook #'xeft-setup)
  (add-hook 'xeft-find-file-hook #'bklink-minor-mode))


;;; init-org.el ends here
(provide 'init-org)
