;;; -*- lexical-binding: t -*-

(straight-use-package 'easy-hugo)
(straight-use-package 'org-superstar)
(straight-use-package 'org-roam)
(straight-use-package 'org-roam-server)
(straight-use-package '(org-transclusion :type git :host github :repo "nobiot/org-transclusion"))
(straight-use-package 'valign)
(straight-use-package 'ob-go)
(straight-use-package 'org-journal)

(+pdump-packages 'easy-hugo
                 'org-superstar
                 'org-roam
                 'org-roam-server
                 'org-transclusion
                 'valign
                 'ob-go
                 'org-journal)

;;; org
(setq
 org-log-done t
 org-directory "~/Dropbox/org"
 org-html-checkbox-type 'unicode
 org-todo-keywords        (quote ((sequence "TODO(t)" "WIP(w/!)" "WAIT(W@/!)" "HOLD(h)" "|" "CANCELLED(c@/!)" "DONE(d!/!)")))
 org-todo-repeat-to-state "NEXT"
 org-todo-keyword-faces   (quote (("NEXT" :inherit warning)
  				                  ("WAIT" :inherit font-lock-string-face))))

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

;;; org-journal
(setq
 org-journal-dir (expand-file-name "journal" org-directory))

;;; org-agenda
(setq
 org-agenda-files (list org-directory)
 org-agenda-diary-file (expand-file-name "diary.org" org-directory))

(global-set-key (kbd "<f12>") 'org-agenda)

;;; org-capture
(global-set-key (kbd "C-c c") 'org-capture)

(setq
 org-default-notes-file (concat org-directory "/cap.org")
 org-capture-templates
 '(("i" "Idea" entry (file org-default-notes-file)
    "*  %^{Title} %?\n%U\n%a\n")
   ("t" "Todo" entry (file org-default-notes-file)
    "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
   ("n" "Note" entry (file org-default-notes-file)
    "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
   ("j" "Journal" entry (file+olp+datetree org-default-notes-file)
    "*  %^{Title} %?\n%U\n%a\n" :clock-in t :clock-resume t)
   ("b" "Book" entry (file+olp+datetree org-default-notes-file)
    "* Topic: %^{Description}  %^g %? Added: %U")))

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
 org-superstar-leading-bullet ?\s
 org-superstar-headline-bullets-list '("♥" "✿" "❀" "☢" "✸" "◉")
 org-superstar-item-bullet-alist '((?* . ?☯) (?+ . ?✚) (?- . ?▶)))

(autoload #'org-superstar-mode "org-superstar")

(add-hook 'org-mode-hook 'org-superstar-mode)

;;; org-roam
(setq org-roam-directory "~/Dropbox/org")

(with-eval-after-load "org-roam"
  (define-key org-roam-mode-map (kbd "C-x C-r l") 'org-roam)
  (define-key org-roam-mode-map (kbd "C-x C-r f") 'org-roam-find-file)
  (define-key org-roam-mode-map (kbd "C-x C-r g") 'org-roam-graph)
  (define-key org-roam-mode-map (kbd "C-x C-r c") 'org-roam-db-build-cache)

  (define-key org-mode-map (kbd "<f7>") 'org-roam-insert)
  (define-key org-mode-map (kbd "C-x C-r i") 'org-roam-insert)
  (define-key org-mode-map (kbd "C-x C-r I") 'org-roam-insert-immediate)

  ;; https://www.orgroam.com/manual.html#Roam-Protocol
  (require 'org-roam-protocol))

;;; TODO org-roam-server

(provide 'init-org)
