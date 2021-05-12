;;; -*- lexical-binding: t -*-

(straight-use-package 'easy-hugo)
(straight-use-package 'org-superstar)
(straight-use-package 'org-roam)
(straight-use-package 'org-roam-server)
(straight-use-package '(org-transclusion :type git :host github :repo "nobiot/org-transclusion"))

;;; org
(setq
 org-log-done t
 org-directory "~/Dropbox/org"
 org-html-checkbox-type 'unicode
 org-todo-keywords        (quote ((sequence "TODO(t)" "WIP(w/!)" "WAIT(W@/!)" "HOLD(h)" "|" "CANCELLED(c@/!)" "DONE(d!/!)")))
 org-todo-repeat-to-state "NEXT"
 org-todo-keyword-faces   (quote (("NEXT" :inherit warning)
  				                  ("WAIT" :inherit font-lock-string-face))))

(with-eval-after-load "org"
  (require 'org-tempo)
  (require 'ob)
  (require 'ob-dot)

  ;; org-transclusion
  (require 'org-transclusion)
  (define-key global-map (kbd "<f11>") #'org-transclusion-mode))

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

;; easy-hugo
(setq
 easy-hugo-server-flags "-D"
 easy-hugo-basedir "~/bookshelf/"
 easy-hugo-previewtime "300"
 easy-hugo-default-ext ".org"
 easy-hugo-org-header t)

(autoload #'easy-hugo "easy-hugo" nil t)

;; org-superstar
(setq
 org-superstar-leading-bullet ?\s
 org-superstar-headline-bullets-list '("♥" "✿" "❀" "☢" "✸" "◉")
 org-superstar-item-bullet-alist '((?* . ?☯) (?+ . ?✚) (?- . ?▶)))

(autoload #'org-superstar-mode "org-superstar")

(add-hook 'org-mode-hook 'org-superstar-mode)

;; org-roam
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

;; TODO org-roam-server

(provide 'init-org)
