;;; -*- lexical-binding: t -*-

(setq
 org-ellipsis " ▾ "
 org-special-ctrl-a/e t
 org-special-ctrl-k t
 org-directory (expand-file-name "~/Dropbox/org")
 org-plantuml-exec-mode 'plantuml
 org-complete-tags-always-offer-all-agenda-tags t
 ;; YYYY-MM-DD
 calendar-date-style 'ios
 ;; Footnotes go into the section they are referenced in
 org-footnote-section nil
 org-footnote-auto-adjust t
 ;; Use return to open link.
 org-return-follows-link t
 ;; perf
 ;; https://emacs-china.org/t/org-babel/18699/12?u=rua
 org-modules nil
 ;; Fold all contents on opening a org file.
 org-startup-folded t
 ;; Always display images.
 org-startup-with-inline-images t
 ;; Always download and display remote images.
 org-display-remote-inline-images 'download
 ;; Do not display image actual width, set to 500px by default.
 org-image-actual-width '(300)
 ;; Add a time stamp when a task change to done.
 org-log-done 'time
 ;; Edit source code in the current window.
 org-edit-src-content-indentation 0
 org-src-window-setup 'current-window)

;; When add http/https link, use title as description
(setq org-make-link-description-function 'my/url-get-title)

(defun my/org-mode-setup ()
  (org-indent-mode 1)
  (electric-pair-local-mode -1)
  (electric-quote-local-mode)
  (electric-indent-local-mode -1)
  (when (display-graphic-p)
    (valign-mode 1)))
(add-hook 'org-mode-hook #'my/org-mode-setup)

(with-eval-after-load 'org
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (require 'org-tempo))

;; utils
;; https://github.com/alphapapa/unpackaged.el#surround-region-with-emphasis-or-syntax-characters
(defmacro org-surround-markup (&rest keys)
  "Define and bind interactive commands for each of KEYS that surround the region or insert text.
Commands are bound in `org-mode-map' to each of KEYS.  If the
region is active, commands surround it with the key character,
otherwise call `org-self-insert-command'."
  `(progn
     ,@(cl-loop for key in keys
                for name = (intern (concat "unpackaged/org-maybe-surround-" key))
                for docstring = (format "If region is active, surround it with \"%s\", otherwise call `org-self-insert-command'." key)
                collect `(defun ,name ()
                           ,docstring
                           (interactive)
                           (if (region-active-p)
                               (let ((beg (region-beginning))
                                     (end (region-end)))
                                 (save-excursion
                                   (goto-char end)
                                   (insert ,key)
                                   (goto-char beg)
                                   (insert ,key)))
                             (call-interactively #'org-self-insert-command)))
                collect `(define-key org-mode-map (kbd ,key) #',name))))
(with-eval-after-load 'org
  (org-surround-markup "*" "/" "_" "=" "+" "$"))

;;; Agenda && Capture
;;
;; For capture and view tasks.

;; Define tags for tasks context.
;; https://systemcrafters.net/org-mode-productivity/effective-task-tags-by-context/
(setq org-tag-alist '(("@feature" . ?f)
                      ("@error" . ?e)
                      ;; weekly
                      ("@reportw" . ?w)
                      ;; monthly
                      ("@reportm" . ?m)
                      ;; quarter
                      ("@reportq" . ?q)))

(setq org-default-notes-file (concat org-directory "/default-notes.org")
      org-capture-templates
      `(("b" "Blog idea" entry (file "~/Dropbox/org/blog.org") "* %^{title}\n%u\n%?" :prepend t)
        ("p" "Project idea" entry (file "~/Dropbox/org/project.org") "* %^{title}\n%u\n%?" :prepend t)
        ("i" "Inbox" entry (file "~/Dropbox/org/inbox.org") "* TODO %?\n:PROPERITIES:\n:Created: %T\n:END:")
        ("n" "Note" entry (file "~/Dropbox/org/roam/Notes.org") "* %^{title}\n%u\n%?" :prepend t)
        ("w" "Work" entry (file+olp+datetree "~/Dropbox/org/Work.org")
         "* %^{Title}\n:PROPERITIES:\n:Created: %T\n:END:" :tree-type week)))
(keymap-global-set "C-c c" 'org-capture)

(setq org-todo-keywords '((sequence "TODO(t)" "WIP(i!)" "WAIT(w!)" "|" "DONE(d!)" "CANCELLED(c@/!)"))
      org-todo-keyword-faces '(("TODO"       :foreground "#7c7c75" :weight bold)
                               ("WIP"        :foreground "#0098dd" :weight bold)
                               ("WAIT"       :foreground "#9f7efe" :weight bold)
                               ("DONE"       :foreground "#50a14f" :weight bold)
                               ("CANCELLED"  :foreground "#ff6480" :weight bold)))
;; Save when I change a workflow state.
(add-hook 'org-trigger-hook 'save-buffer)

(setq org-agenda-files (list org-directory)
      org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                                 (todo   . " ")
                                 (tags   . " %i %-12:c")
                                 (search . " %i %-12:c"))
      ;; hide any tag
      org-agenda-hide-tags-regexp "."
      org-agenda-current-time-string
      "⭠ now ─────────────────────────────────────────────────")

;; Do not reorganize-frame
(setq org-agenda-window-setup 'current-window)

(keymap-global-set "C-c a" 'org-agenda)
(keymap-global-set "C-c l" 'org-store-link)

;;; LaTeX
;;
;; For export org to pdf.
(setq org-latex-compiler "xelatex")
(setq org-preview-latex-default-process 'dvisvgm)
(setq org-preview-latex-image-directory "~/.cache/org-latex")
(setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "rm -fr %b.out %b.log %b.tex auto"))
(setq org-latex-packages-alist '("\\usepackage[UTF8, fontset=fandol]{ctex}"))

;;; org-babel
;;
;; Execute code block in org file.

;; No confirm when execute code block.
(setq org-confirm-babel-evaluate nil)

(add-hook 'org-babel-after-execute #'org-redisplay-inline-images)

;; https://emacs-china.org/t/org-babel/18699/10?u=rua
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

(install-package 'corg "https://github.com/isamert/corg.el")
(add-hook 'org-mode-hook #'corg-setup)

(install-package 'ob-go)

;;; Export to html
;;
;; For now use '$pandoc --embed-resources --standalone' .
(defun my/org-export-to-html ()
  "Convert current org buffer to html with image embed.
Need pandoc installed."
  (interactive)
  (let* ((from (buffer-file-name))
         (to (concat (file-name-sans-extension from) ".html")))
    (shell-command (format "pandoc --embed-resources --standalone %s -o %s" from to))
    (find-file to)))

(install-package 'htmlize)
(install-package 'ox-gfm)
(with-eval-after-load 'org
  (add-to-list 'org-export-backends 'md))

;;; citar
;;
;; Insert citation in org-mode.
(install-package 'citar)
(setq org-cite-global-bibliography '("~/Dropbox/bib/references.bib")
      org-cite-insert-processor 'citar
      org-cite-follow-processor 'citar
      org-cite-activate-processor 'citar
      citar-bibliography org-cite-global-bibliography)

;;; Blog
(install-package 'org-static-blog)
(setq org-static-blog-publish-title ""
      org-static-blog-publish-url "https://lakkiy.github.io/"
      org-static-blog-publish-directory "~/p/blog/"
      org-static-blog-posts-directory "~/Dropbox/my/blog/posts/"
      org-static-blog-drafts-directory "~/Dropbox/my/blog/drafts/"
      org-static-blog-enable-tags t)
(with-eval-after-load 'org-static-blog
  (setq org-static-blog-page-header (get-string-from-file "~/p/blog/static/header.html")
        org-static-blog-page-preamble (get-string-from-file "~/p/blog/static/preamble.html")
        org-static-blog-page-postamble (get-string-from-file "~/p/blog/static/postamble.html")))

;;; Slide
;;
;; Make slide with org-mode.

;; TODO config font and center
(install-package 'dslide)

;;; Look And Feel

;; Toggles visibility of hidden org-mode element parts upon entering and leaving an element.
(install-package 'org-appear)
(setq org-hide-emphasis-markers t)
(add-hook 'org-mode-hook #'org-appear-mode)

;; org-modern
(install-package 'org-modern)
(setq org-modern-star ["›"]
      ;; Enable this will break code block indentation.
      org-modern-block-fringe nil
      ;; use valign instead
      org-modern-table nil)

(defun my/setup-org-modern ()
  (setq-local line-spacing 0.15)
  (org-modern-mode))
(add-hook 'org-mode-hook 'my/setup-org-modern)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

;; Make `org-indent-mode' work with org-modern
(install-package 'org-modern-indent "https://github.com/jdtsmith/org-modern-indent")
(setq org-modern-hide-stars nil
      org-modern-block-name '("" . ""))
(add-hook 'org-mode-hook 'org-modern-indent-mode 90)

;;; init-org.el ends here
