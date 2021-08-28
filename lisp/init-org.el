;;; -*- lexical-binding: t -*-

(eat-package restclient
  :straight t
  :commands restclient-mode)

(eat-package ob-restclient
  :straight t
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

(eat-package pretty-hydra :straight t)

(eat-package org
  :hook (org-mode-hook . visual-line-mode)
  :init
  (setq org-directory "~/Dropbox/org")
  :config
  (setq org-startup-indented t
        org-hide-emphasis-markers t
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
  (defun hot-expand (str &optional mod)
    "Expand org template.

STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
      (insert str)
      (if (fboundp 'org-try-structure-completion)
          (org-try-structure-completion) ; < org 9
        (progn
          ;; New template expansion since org 9
          (require 'org-tempo nil t)
          (org-tempo-complete-tag)))
      (when mod (insert mod) (forward-line))
      (when text (insert text))))
  (pretty-hydra-define org-hydra (:title "Org Template" :quit-key "q")
    ("Basic"
     (("a" (hot-expand "<a") "ascii")
      ("c" (hot-expand "<c") "center")
      ("C" (hot-expand "<C") "comment")
      ("e" (hot-expand "<e") "example")
      ("E" (hot-expand "<E") "export")
      ("h" (hot-expand "<h") "html")
      ("l" (hot-expand "<l") "latex")
      ("n" (hot-expand "<n") "note")
      ("o" (hot-expand "<q") "quote")
      ("v" (hot-expand "<v") "verse"))
     "Head"
     (("i" (hot-expand "<i") "index")
      ("A" (hot-expand "<A") "ASCII")
      ("I" (hot-expand "<I") "INCLUDE")
      ("H" (hot-expand "<H") "HTML")
      ("L" (hot-expand "<L") "LaTeX"))
     "Source"
     (("s" (hot-expand "<s") "src")
      ("m" (hot-expand "<s" "emacs-lisp") "emacs-lisp")
      ("y" (hot-expand "<s" "python :results output") "python")
      ("p" (hot-expand "<s" "perl") "perl")
      ("r" (hot-expand "<s" "ruby") "ruby")
      ("S" (hot-expand "<s" "sh") "sh")
      ("j" (hot-expand "<s" "js") "javescript")
      ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)") "golang"))
     "Misc"
     (("u" (hot-expand "<s" "plantuml :file CHANGE.png") "plantuml")
      ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0") "ipython")
      ("P" (progn
             (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
             (hot-expand "<s" "perl")) "Perl tangled")
      ("<" self-insert-command "ins"))))
  ;; For hydra
  (require 'org-tempo)
  (require 'ob)
  (require 'ob-dot)
  (define-key org-mode-map (kbd "<") (lambda ()
                                       "Insert org template."
                                       (interactive)
                                       (if (or (region-active-p) (looking-back "^\s*" 1))
                                           (org-hydra/body)
                                         (self-insert-command 1)))))

(eat-package ob-go
  :straight t
  :after org)

(when (display-graphic-p)
  (eat-package valign
    :straight t
    :after org
    :hook (org-mode-hook . valign-mode)))

(eat-package doct
  :straight t
  :doc "Declarative Org Capture Templates"
  :commandes doct)

(eat-package org-journal
  :straight t
  :init
  (setq org-journal-file-type 'yearly
        org-journal-dir (concat org-directory "/journal")
        org-journal-file-format "%Y"
        org-journal-date-format "%Y 年 %m 月 %d 日 %A")
  (defun org-journal-find-location ()
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    ;; Position point on the journal's top-level heading so that org-capture
    ;; will add the new entry as a child entry.
    (goto-char (point-min))))

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

  (setq org-default-notes-file (concat org-directory "/default-notes.org")
        org-capture-templates
        (doct '(("Work" :keys "w" :file "~/Dropbox/org/Work.org"
                 :datetree t
                 :tree-type week
                 :template ("* %^{Description}"
                            ":PROPERITIES:"
                            ":Created: %T" ;; used to create weekly report
                            ":Project: "
                            ":Branch: "
                            ":Jira: "
                            ":END:"))
                ("Journal" :keys "j"
                 :function (lambda () (org-journal-find-location))
                 :clock-in t :clock-resume t
                 :template ("* %(format-time-string org-journal-time-format) %^{Title}"
                            "  %i%?"))
                ("Billing" :keys "b" :type plain :file "~/Dropbox/org/Billing.org"
                 :function (lambda () (find-month-tree))
                 :template (" | %U | %^{Category} | %^{Description} | %^{Amount} |"))
                ("Schedule" :keys "s" :file "~/Dropbox/org/Schedule.org"
                 :datetree t
                 :template ("* %^{Description}"
                            ":PROPERTIES:"
                            ":Created: %U"
                            ":END:"))
                ("Web site" :keys "e" :file "~/Dropbox/org/Notes.org"
                 :headline "Inbox"
                 :template ("* %^{Title} :website:"
                            ":PROPERTIES:"
                            ":Created: %U"
                            ":END:"
                            "%?%:initial"))
                (:group "All Notes"
                        :file "~/Dropbox/org/Notes.org"
                        :template ("* %^{Description}"
                                   ":PROPERTIES:"
                                   ":Created: %U"
                                   ":END:"
                                   "%?")
                        :children
                        (("Notes" :keys "n" :olp ("Notes")
                          :datetree t)
                         ("Exercise" :keys "e" :olp ("Exercise"))
                         ("Research" :keys "n" :olp ("Research")
                          :clock-in t :clock-resume t :prepend t)
                         ("Computer" :keys "c"
                          :prepend t
                          :children
                          (("Emacs" :keys "e" :olp ("Computer" "Emacs"))
                           ("Linux" :keys "l" :olp ("Computer" "Linux"))
                           ("Golang" :keys "g" :olp ("Computer" "Golang"))
                           ("Python" :keys "p" :olp ("Computer" "Python"))
                           ("Windows" :keys "w" :olp ("Computer" "Windows"))))))
                ("Tasks" :keys "t" :file "~/Dropbox/org/Tasks.org"
                 :template ("* %{todo-state} %^{Description}"
                            ":PROPERTIES:"
                            ":Created: %U"
                            ":END:")
                 :children
                 (("Computer"
                   :keys "c" :headline "Computer" :todo-state "TODO")
                  ("Food"
                   :keys "f" :headline "Food" :todo-state "TODO")
                  ("Research"
                   :keys "r" :headline "Research" :todo-state "TODO")
                  ("Idea"
                   :keys "i" :headline "Idea" :todo-state "TODO")
                  ("Not grouped"
                   :keys "n" :headline "Not grouped" :todo-state "TODO")
                  ("Books"
                   :keys "b" :headline "Book" :todo-state "TODO")))))))

(eat-package easy-hugo
  :straight t
  :commands easy-hugo
  :init
  (setq easy-hugo-server-flags "-D"
        easy-hugo-basedir "~/bookshelf/"
        easy-hugo-previewtime "300"
        easy-hugo-default-ext ".org"
        easy-hugo-org-header t))

(eat-package org-roam
  :straight t
  :init
  (eat-package emacsql-sqlite :straight t)
  ;; for org-roam-buffer-toggle
  ;; Use side-window like V1
  ;; This can take advantage of slots available with it
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.25)
                 (preserve-size . (t nil))
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))
  (setq org-roam-v2-ack t
        org-roam-capture-templates '(("d" "default" plain "* %?"
                                      :if-new (file+head "%<Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags:")
                                      :clock-in t
                                      :clock-resume t
                                      :unnarrowed t)
                                     ("l" "leetcode" plain "%?"
                                      :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                                             "#+title: ${title}\n#+filetags:"
                                                             ("References\n* Description\n* Code\n* Time & Space\n* Related & Recommend"))
                                      :clock-in t
                                      :clock-resume t
                                      :unnarrowed t))
        org-roam-directory (let ((p (expand-file-name (concat org-directory "/roam"))))
                             (unless (file-directory-p p) (make-directory p))
                             p))
  (global-set-key (kbd "C-c n l") 'org-roam-buffer-toggle)
  (global-set-key (kbd "C-c n f") 'org-roam-node-find)
  (global-set-key (kbd "C-c n g") 'org-roam-graph)
  (global-set-key (kbd "C-c n i") 'org-roam-node-insert)
  (global-set-key (kbd "C-c n c") 'org-roam-capture)
  (global-set-key (kbd "C-c n j") 'org-roam-dailies-capture-today)
  (global-set-key (kbd "C-c n s") 'org-roam-db-sync)
  :config
  (require 'org-roam-protocol))

(eat-package org-roam-ui
  :straight
  (org-roam-ui :type git :host github :repo "org-roam/org-roam-ui"
               :branch "main" :files ("*.el" "out")))

(eat-package org-agenda
  :init
  (setq org-agenda-files (list org-directory org-roam-directory))
  (global-set-key (kbd "C-c a") 'org-agenda))

(eat-package org-download
  :straight t
  :commands
  org-download-clipboard
  org-download-yank
  org-download-screenshot
  :init
  (setq-default org-download-image-dir (concat org-directory "/pictures"))
  (setq org-download-image-org-width 800
        org-download-backend "curl"
        org-download-screenshot-method (cond (sys/macp "screencapture -ci")
                                             (sys/linuxp "flameshot gui --raw > %s")
                                             (t ""))))

(eat-package org-media-note
  :straight
  (org-media-note :type git :host github :repo "yuchen-lea/org-media-note")
  :hook (org-mode-hook . org-media-note-mode)
  :init
  (setq org-media-note-screenshot-image-dir "~/Dropbox/org/media-note-imgs/"))

(eat-package toc-org
  :straight t
  :commands toc-org-enable toc-org-insert-toc)

(eat-package olivetti
  :straight t
  :commands olivetti-mode)

(defvar eat-prose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-a") #'beginning-of-visual-line)
    (define-key map (kbd "C-e") #'end-of-visual-line)
    map)
  "Mode map for ‘eat-prose-mode’.")

(define-minor-mode eat-prose-mode
  "A mode that optimizes for prose editing."
  :lighter " PROSE"
  :keymap eat-prose-mode-map
  (if eat-prose-mode
      (progn
        (variable-pitch-mode)
        (olivetti-mode)
        (electric-pair-local-mode -1)
        (electric-quote-local-mode)
        (setq-local cursor-type 'bar)
        (setq-local line-spacing 0.15)
        (company-mode -1)
        (setq-local whitespace-style '(tab-mark))
        (whitespace-mode))
    (whitespace-mode -1)
    (company-mode)
    (variable-pitch-mode -1)
    (olivetti-mode -1)
    (electric-pair-local-mode)
    (electric-quote-local-mode -1)
    (kill-local-variable 'line-spacing)
    (kill-local-variable 'cursor-type)))

(provide 'init-org)
