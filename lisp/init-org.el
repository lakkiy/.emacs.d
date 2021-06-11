;;; -*- lexical-binding: t -*-

(straight-use-package 'easy-hugo)
(straight-use-package 'org-superstar)
(straight-use-package 'org-roam)
(straight-use-package 'org-roam-server)
(straight-use-package '(org-transclusion :type git :host github :repo "nobiot/org-transclusion"))
(straight-use-package 'valign)
(straight-use-package 'ob-go)
(straight-use-package 'org-super-agenda)
(straight-use-package 'doct)
(straight-use-package 'org-journal)
(straight-use-package 'restclient)
(straight-use-package 'ob-restclient)

(+pdump-packages 'easy-hugo
                 'restclient
                 'ob-restclient
                 'org-journal
                 'doct
                 'org-superstar
                 'org-roam
                 'org-super-agenda
                 'org-roam-server
                 'org-roam-bibtex
                 'org-transclusion
                 'valign
                 'ob-go)

;; restclient
(autoload 'restclient-mode "restclient" nil t)

;;; org
(setq
 ;; hide markup for =monospace=, ~code~, /italic/, *bold* etc.
 org-hide-emphasis-markers t
 ;; fontify code in code blocks
 org-src-fontify-natively t
 ;; place tags directly after headline text, with only one space in between
 org-tags-column 0
 ;; Highlight latex text in org mode
 org-highlight-latex-and-related '(latex script entities)
 org-src-window-setup 'current-window
 org-log-done t
 org-directory "~/Dropbox/org"
 org-html-checkbox-type 'unicode
 org-todo-keywords        (quote ((sequence "TODO(t)" "WIP(w/!)" "WAIT(W@/!)" "HOLD(h)" "|" "CANCELLED(c@/!)" "DONE(d!/!)")))
 org-todo-repeat-to-state "NEXT"
 org-todo-keyword-faces   (quote (("NEXT" :inherit warning)
  				                  ("WAIT" :inherit font-lock-string-face))))

(defun +org-insert-link-dwim ()
  "Like `org-insert-link' but with personal dwim preferences."
  (interactive)
  (let* ((point-in-link (org-in-regexp org-link-any-re 1))
         (clipboard-url (when (string-match-p "^http" (current-kill 0))
                          (current-kill 0)))
         (region-content (when (region-active-p)
                           (buffer-substring-no-properties (region-beginning)
                                                           (region-end)))))
    (cond ((and region-content clipboard-url (not point-in-link))
           (delete-region (region-beginning) (region-end))
           (insert (org-make-link-string clipboard-url region-content)))
          ((and clipboard-url (not point-in-link))
           (insert (org-make-link-string
                    clipboard-url
                    (read-string "title: "
                                 (with-current-buffer (url-retrieve-synchronously clipboard-url)
                                   (dom-text (car
                                              (dom-by-tag (libxml-parse-html-region
                                                           (point-min)
                                                           (point-max))
                                                          'title))))))))
          (t
           (call-interactively 'org-insert-link)))))

;; For hydra
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
     ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)") "golang"))
    "Misc"
    (("u" (hot-expand "<s" "plantuml :file CHANGE.png") "plantuml")
     ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0") "ipython")
     ("P" (progn
            (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
            (hot-expand "<s" "perl")) "Perl tangled")
     ("<" self-insert-command "ins"))))

(with-eval-after-load "org"
  ;;; valign
  (add-hook 'org-mode-hook #'valign-mode)
  (add-hook 'org-mode-hook #'visual-line-mode)

  (require 'org-tempo)
  (require 'ob)
  (require 'ob-dot)
  (require 'ob-go)

  ;; org-transclusion
  (require 'org-transclusion)
  (define-key global-map (kbd "<f11>") #'org-transclusion-mode)
  (define-key org-mode-map (kbd "<") (lambda ()
                                       "Insert org template."
                                       (interactive)
                                       (if (or (region-active-p) (looking-back "^\s*" 1))
                                           (org-hydra/body)
                                         (self-insert-command 1)))))

;;; org-agenda
(setq
 org-agenda-files (list org-directory))

(global-set-key (kbd "C-c a") 'org-agenda)

;; TODO hydra for org agenda
;; TODO group journal and not display in default

;;; org super agenda
(add-hook 'org-agenda-mode 'org-super-agenda-mode)

(setq
 org-super-agenda-group nil)

;;; doct : Declarative Org Capture Templates
(autoload 'doct "doct")

;;; org-journal
(setq
 org-journal-file-type 'yearly
 org-journal-dir (concat org-directory "/journal/")
 org-journal-file-format "%Y"
 org-journal-date-format "%Y 年 %m 月 %d 日 %A")

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))

;;; org-capture
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
 (doct '(("work" :keys "w" :file "~/Dropbox/org/work.org"
          :datetree t
          :tree-type week
          :template ("* %^{Description}"
                     ":PROPERITIES:"
                     ":Created: %T" ;; used to create weekly report
                     ":END:"))
         ("journal" :keys "j"
          :function (lambda () (org-journal-find-location))
          :clock-in t :clock-resume t
          :template ("* %(format-time-string org-journal-time-format) %^{Title}"
                     "  %i%?"))
         ("billing" :keys "b" :type plain :file "~/Dropbox/org/billing.org"
          :function (lambda () (find-month-tree))
          :template (" | %U | %^{类别} | %^{描述} | %^{金额} |")))))

;;; easy-hugo
(setq
 easy-hugo-server-flags "-D"
 easy-hugo-basedir "~/bookshelf/"
 easy-hugo-previewtime "300"
 easy-hugo-default-ext ".org"
 easy-hugo-org-header t)

(autoload #'easy-hugo "easy-hugo" nil t)

;;; org-superstar
(setq
 org-superstar-leading-bullet ?\s)

(autoload #'org-superstar-mode "org-superstar")

(add-hook 'org-mode-hook 'org-superstar-mode)

;;; org-roam
(setq
 org-roam-directory
  (let ((p (expand-file-name (concat org-directory "/roam"))))
   (unless (file-directory-p p) (make-directory p))
   p))

(add-hook 'after-init-hook 'org-roam-mode)

(with-eval-after-load "org-roam"
  (define-key org-roam-mode-map (kbd "C-x C-r l") 'org-roam)
  (define-key org-roam-mode-map (kbd "C-x C-r f") 'org-roam-find-file)
  (define-key org-roam-mode-map (kbd "C-x C-r g") 'org-roam-graph)
  (define-key org-roam-mode-map (kbd "C-x C-r c") 'org-roam-db-build-cache)

  (define-key org-mode-map (kbd "C-x C-r i") 'org-roam-insert)
  (define-key org-mode-map (kbd "C-x C-r I") 'org-roam-insert-immediate)

  ;; https://www.orgroam.com/manual.html#Roam-Protocol
  (require 'org-roam-protocol))

(provide 'init-org)
