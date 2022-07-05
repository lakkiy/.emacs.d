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
  :init
  (setq org-directory "~/Dropbox/org")

  (defvar load-language-list '((emacs-lisp . t)
                               (python . t)
                               (js . t)
                               (C . t)
                               (shell . t)))

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
  (setq
   ;; Faster loading
   org-modules nil
   ;; run src block without confirm
   org-confirm-babel-evaluate nil
   ;; use #+attr_org :width 300px to rescale
   org-image-actual-width nil
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
   org-log-done t)

  (require 'org-tempo) ;; see `org-structure-template-alist'
  (require 'ob)
  (require 'ob-dot)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (org-babel-do-load-languages 'org-babel-load-languages load-language-list))

(eat-package org-capture
  :init
  (global-set-key (kbd "C-c c") 'org-capture)

  (setq
   org-default-notes-file (concat org-directory "/default-notes.org")
   org-capture-templates
   `(("w" "Work" entry (file+olp+datetree "~/Dropbox/org/Work.org")
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

(when (display-graphic-p)
  (eat-package valign
    :straight t
    :hook (org-mode-hook . valign-mode)
    :init
    (setq valign-fancy-bar t)))

(eat-package toc-org
  :straight t
  :commands toc-org-enable toc-org-insert-toc)

;;; Writing

(eat-package iimg
  ;; FIXME hook is nil
  :hook (on-first-file-hook . iimg-enable))

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
             :files (:defaults "Makefile" "module"))
  :init
  (setq xeft-directory "~/Dropbox/org/roam"
        xeft-database "~/.xeft/db")
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
