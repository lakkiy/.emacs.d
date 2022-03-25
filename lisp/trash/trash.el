

;; some times it's just not work
(eat-package insert-translated-name
  :straight (insert-translated-name :type git
                                    :host github
                                    :repo "manateelazycat/insert-translated-name")
  :commands insert-translated-name-insert
  :init
  (global-set-key (kbd "C-c i") 'insert-translated-name-insert))

;; maybe it will return to my config
(eat-package elisp-demos
  :straight t
  :init
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;; for now, `fanyi' works well, maybe need this where there is no network
(eat-package sdcv
  :straight (sdcv :type git :host github :repo "manateelazycat/sdcv")
  :commands
  sdcv-search-pointer
  sdcv-search-pointer+
  sdcv-search-input
  sdcv-search-input+
  :init
  (setq sdcv-dictionary-data-dir (file-truename "~/.sdcv-dict")
        sdcv-dictionary-simple-list
        '("懒虫简明英汉词典"
          "懒虫简明汉英词典"
          "KDic11万英汉词典")
        sdcv-dictionary-complete-list
        '("懒虫简明英汉词典"
          "英汉汉英专业词典"
          "XDICT英汉辞典"
          "stardict1.3英汉辞典"
          "WordNet"
          "XDICT汉英辞典"
          "懒虫简明汉英词典"
          "新世纪英汉科技大词典"
          "KDic11万英汉词典"
          "朗道汉英字典5.0"
          "CDICT5英汉辞典"
          "新世纪汉英科技大词典"
          "牛津英汉双解美化版"
          "21世纪双语科技词典"
          "quick_eng-zh_CN"))
  (defun sdcv-dwim (word)
    "Translate WORD."
    (interactive (let* ((default (if (use-region-p)
                                     (buffer-substring-no-properties (region-beginning) (region-end))
                                   (thing-at-point 'word t)))
                        (prompt (if (stringp default)
                                    (format "Search Word (default \"%s\"): " default)
                                  "Search Word: ")))
                   (list (read-string prompt nil nil default))))
    (sdcv-search-input word))
  (global-set-key (kbd "C-c Y") #'sdcv-dwim))

;; limit eldoc line num
(eat-package eldoc-overlay :straight t)

(eat-package turbo-log
  :straight (turbo-log :host github :repo "artawower/turbo-log.el")
  :init
  (global-set-key (kbd "C-s-h") #'turbo-log-print-immediately)
  (global-set-key (kbd "C-s-g") #'turbo-log-delete-all-logs))

(eat-package ibuffer-project
  :straight t
  :init
  ;; use `ibuffer-project-clear-cache' to clear cache
  (setq ibuffer-project-use-cache t)
  (custom-set-variables
   '(ibuffer-formats
     '((mark modified read-only locked " "
             (name 18 18 :left :elide)
             " "
             (size 9 -1 :right)
             " "
             (mode 16 16 :left :elide)
             " " project-file-relative))))
  (add-hook 'ibuffer-hook
            (lambda ()
              (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
              (unless (eq ibuffer-sorting-mode 'project-file-relative)
                (ibuffer-do-sort-by-project-file-relative))))
  :config
  ;; In this case all remote buffers will be grouped by a string identifying the remote connection.
  (add-to-list 'ibuffer-project-root-functions '(file-remote-p . "Remote")))

(straight-use-package '(dired-hacks :type git :host github :repo "Fuco1/dired-hacks"))

(eat-package dired-filter
  :hook (dired-mode-hook . dired-filter-group-mode)
  :init
  (setq dired-filter-revert 'never
        dired-filter-group-saved-groups
        '(("default"
           ("Git"
            (directory . ".git")
            (file . ".gitignore"))
           ("Directory"
            (directory))
           ("PDF"
            (extension . "pdf"))
           ("LaTeX"
            (extension "tex" "bib"))
           ("Source"
            (extension "c" "cpp" "hs" "rb" "py" "r" "cs" "el" "lisp" "html" "js" "css"))
           ("Doc"
            (extension "md" "rst" "txt"))
           ("Org"
            (extension . "org"))
           ("Archives"
            (extension "zip" "rar" "gz" "bz2" "tar"))
           ("Images"
            (extension "jpg" "JPG" "webp" "png" "PNG" "jpeg" "JPEG" "bmp" "BMP" "TIFF" "tiff" "gif" "GIF")))))
  :config
  (define-key dired-filter-map (kbd "p") 'dired-filter-pop-all)
  (define-key dired-filter-map (kbd "/") 'dired-filter-mark-map))

(eat-package dired-collapse
  :hook (dired-mode-hook . dired-collapse-mode))


(eat-package pretty-hydra
  :straight t
  :init
  (with-eval-after-load 'org
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

    (define-key org-mode-map (kbd "<")
                (lambda ()
                  "Insert org template."
                  (interactive)
                  (if (or (region-active-p) (looking-back "^\s*" 1))
                      (org-hydra/body)
                    (self-insert-command 1))))))

(eat-package auctex :straight t)

(eat-package citar
  :straight (citar :type git :host github :repo "bdarcus/citar")
  :init
  (setq citar-bibliography '("~/Dropbox/bib/references.bib")))

;; Better to have title name with project name
(setq-default frame-title-format
              '((:eval
                 (or (cdr (project-current))
                     (buffer-name)))))

;; FIXME this func cause require org on stratup
(defun org-agenda-export-to-ics ()
  (interactive)
  ;; Run all custom agenda commands that have a file argument.
  (org-batch-store-agenda-views)

  ;; Org mode correctly exports TODO keywords as VTODO events in ICS.
  ;; However, some proprietary calendars do not really work with
  ;; standards (looking at you Google), so VTODO is ignored and only
  ;; VEVENT is read.
  (with-current-buffer (find-file-noselect org-agenda-private-local-path)
    (goto-char (point-min))
    (while (re-search-forward "VTODO" nil t)
      (replace-match "VEVENT"))
    (save-buffer))

  ;; Copy the ICS file to a remote server (Tramp paths work).
  (copy-file org-agenda-private-local-path org-agenda-private-remote-path t))

;; this make magit don't refresh
(eat-package magit-todos
  :straight t
  :after magit
  :init
  (setq magit-todos-nice (if (executable-find "nice") t nil))
  :config
  (let ((inhibit-message t))
    (magit-todos-mode 1)))

(eat-package zeft
  :straight (zeft :type git :host github :repo "casouri/zeft")
  :init
  (setq zeft-directory (expand-file-name "~/Dropbox/org/roam")))

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
        org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
          ("l" "leetcode" plain "%?"
           :target (file+head+olp
                    "%<%Y%m%d%H%M%S>-${slug}.org"
                    "#+title: ${title}\n#+filetags:"
                    ("References\n* Description\n* Code\n* Time & Space\n* Related & Recommend"))
           :clock-in t :clock-resume t :unnarrowed t))
        org-roam-directory (let ((p (expand-file-name (concat org-directory "/roam"))))
                             (unless (file-directory-p p) (make-directory p))
                             p))
  (add-to-list 'org-agenda-files org-roam-directory)
  (global-set-key (kbd "C-c n l") 'org-roam-buffer-toggle)
  (global-set-key (kbd "C-c n f") 'org-roam-node-find)
  (global-set-key (kbd "C-c n g") 'org-roam-graph)
  (global-set-key (kbd "C-c n i") 'org-roam-node-insert)
  (global-set-key (kbd "C-c n c") 'org-roam-capture)
  (global-set-key (kbd "C-c n j") 'org-roam-dailies-capture-today)
  (global-set-key (kbd "C-c n s") 'org-roam-db-sync)
  :config
  (require 'org-roam-protocol)
  (org-roam-setup))

(eat-package org-roam-ui
  :straight
  (org-roam-ui :type git :host github :repo "org-roam/org-roam-ui"
               :branch "main" :files ("*.el" "out")))

(eat-package org-download
  :straight t
  :commands
  org-download-clipboard
  org-download-yank
  org-download-screenshot
  :init
  (setq-default org-download-image-dir (concat org-directory "/pictures"))
  (setq org-download-image-org-width 300
        org-download-backend "curl"
        org-download-screenshot-method (cond (*is-a-mac* "screencapture -ci")
                                             (sys/linuxp "flameshot gui --raw > %s")
                                             (t ""))))
