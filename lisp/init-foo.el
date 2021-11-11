;;; -*- lexical-binding: t -*-

(eat-package vertico-posframe
  :after vertico
  :straight (vertico-posframe :type git :host github :repo "tumashu/vertico-posframe")
  :init
  (when (display-graphic-p)
    (vertico-posframe-mode 1)))

(eat-package doom-modeline
  :straight t
  :init
  (defvar +doom-modeline-p nil
    "Enable `doom-modeline'.")

  (setq doom-modeline-irc nil
        doom-modeline-mu4e nil
        doom-modeline-gnus nil
        doom-modeline-github nil
        doom-modeline-persp-name nil
        doom-modeline-unicode-fallback t
        doom-modeline-enable-work-count nil)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-project-detection 'project)

  (when +doom-modeline-p
    (setq-default mode-line-format nil
                  header-line-format nil))
  (add-hook 'after-init-hook 'doom-modeline-mode))

(run-with-idle-timer
 1 nil
 #'(lambda ()
     (eat-package all-the-icons
       :straight t
       :init
       ;; From centaur-emacs, Support more icons
       (let ((extension-icon-alist
              '(("conf" all-the-icons-octicon "settings"    :v-adjust 0.0 :face all-the-icons-yellow)
                ("eln"  all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-dsilver)
                ("epub" all-the-icons-faicon "book"         :height 1.0 :v-adjust -0.1 :face all-the-icons-green)
                ("make" all-the-icons-fileicon "gnu"        :face all-the-icons-dorange)
                ("rss"  all-the-icons-octicon "rss"         :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange)
                ("toml" all-the-icons-octicon "settings"    :v-adjust 0.0 :face all-the-icons-yellow)
                ("tsx"  all-the-icons-fileicon "tsx"        :height 1.0 :v-adjust -0.1 :face all-the-icons-cyan-alt)
                ("xpm"  all-the-icons-octicon "file-media"  :v-adjust 0.0 :face all-the-icons-dgreen))))
         (dolist (icon extension-icon-alist)
           (add-to-list 'all-the-icons-extension-icon-alist icon)))

       (let ((regexp-icon-alist
              '(("Cask\\'"             all-the-icons-fileicon "elisp"      :height 1.0 :v-adjust -0.2 :face all-the-icons-blue)
                ("^Rakefile$"          all-the-icons-alltheicon "ruby-alt" :face all-the-icons-red)
                ("\\.\\(bat\\|cmd\\)$" all-the-icons-alltheicon "terminal" :face all-the-icons-lsilver)
                ("\\go.mod$"           all-the-icons-fileicon "go"         :face all-the-icons-dblue)
                ("\\go.sum$"           all-the-icons-fileicon "go"         :face all-the-icons-dpurple)
                ("\\.[bB][iI][nN]$"    all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-yellow)
                ("NEWS$"               all-the-icons-faicon "newspaper-o"  :height 0.9 :v-adjust -0.2))))
         (dolist (icon regexp-icon-alist)
           (add-to-list 'all-the-icons-regexp-icon-alist icon)))

       (let ((mode-icon-alist
              '((xwidget-webkit-mode           all-the-icons-faicon "chrome"          :v-adjust -0.1 :face all-the-icons-blue)
                (bongo-playlist-mode           all-the-icons-material "queue_music"   :height 1.2 :face all-the-icons-green)
                (bongo-library-mode            all-the-icons-material "library_music" :height 1.1 :face all-the-icons-green)
                (gnus-group-mode               all-the-icons-fileicon "gnu"           :face all-the-icons-silver)
                (gnus-summary-mode             all-the-icons-octicon "inbox"          :height 1.0 :v-adjust 0.0 :face all-the-icons-orange)
                (gnus-article-mode             all-the-icons-octicon "mail"           :height 1.1 :v-adjust 0.0 :face all-the-icons-lblue)
                (message-mode                  all-the-icons-octicon "mail"           :height 1.1 :v-adjust 0.0 :face all-the-icons-lblue)
                (diff-mode                     all-the-icons-octicon "git-compare"    :v-adjust 0.0 :face all-the-icons-lred)
                (flycheck-error-list-mode      all-the-icons-octicon "checklist"      :height 1.1 :v-adjust 0.0 :face all-the-icons-lred)
                (elfeed-search-mode            all-the-icons-faicon "rss-square"      :v-adjust -0.1 :face all-the-icons-orange)
                (elfeed-show-mode              all-the-icons-octicon "rss"            :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange)
                (newsticker-mode               all-the-icons-faicon "rss-square"      :v-adjust -0.1 :face all-the-icons-orange)
                (newsticker-treeview-mode      all-the-icons-faicon "rss-square"      :v-adjust -0.1 :face all-the-icons-orange)
                (newsticker-treeview-list-mode all-the-icons-octicon "rss"            :height 1.1 :v-adjust 0.0 :face all-the-icons-orange)
                (newsticker-treeview-item-mode all-the-icons-octicon "rss"            :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange)
                (conf-mode                     all-the-icons-octicon "settings"       :v-adjust 0.0 :face all-the-icons-yellow)
                (conf-space-mode               all-the-icons-octicon "settings"       :v-adjust 0.0 :face all-the-icons-yellow)
                (forge-topic-mode              all-the-icons-alltheicon "git"         :face all-the-icons-blue)
                (help-mode                     all-the-icons-faicon "info-circle"     :height 1.1 :v-adjust -0.1 :face all-the-icons-purple)
                (helpful-mode                  all-the-icons-faicon "info-circle"     :height 1.1 :v-adjust -0.1 :face all-the-icons-purple)
                (Info-mode                     all-the-icons-faicon "info-circle"     :height 1.1 :v-adjust -0.1)
                (cask-mode                     all-the-icons-fileicon "elisp"         :height 1.0 :v-adjust -0.2 :face all-the-icons-blue)
                (ein:notebooklist-mode         all-the-icons-faicon "book"            :face all-the-icons-lorange)
                (ein:notebook-mode             all-the-icons-fileicon "jupyter"       :height 1.2 :face all-the-icons-orange)
                (ein:notebook-multilang-mode   all-the-icons-fileicon "jupyter"       :height 1.2 :face all-the-icons-dorange)
                (nov-mode                      all-the-icons-faicon "book"            :height 1.0 :v-adjust -0.1 :face all-the-icons-green)
                (gfm-mode                      all-the-icons-octicon "markdown"       :face all-the-icons-lblue)
                (osx-dictionary-mode           all-the-icons-material "library_books" :face all-the-icons-lblue)
                (youdao-dictionary-mode        all-the-icons-material "library_books" :face all-the-icons-lblue)
                (fanyi-mode                    all-the-icons-material "library_books" :face all-the-icons-lblue))))
         (dolist (icon mode-icon-alist)
           (add-to-list 'all-the-icons-mode-icon-alist icon))))

     (eat-package all-the-icons-ibuffer
       :after ibuffer
       :straight t
       :hook (ibuffer-mode-hook . all-the-icons-ibuffer-mode))

     (eat-package all-the-icons-dired
       :straight t
       :hook (dired-mode-hook . all-the-icons-dired-mode))

     (eat-package treemacs
       :init
       (setq treemacs-no-png-images nil))

     (eat-package all-the-icons-completion
       :straight (all-the-icons-completion :type git :host github :repo "iyefrat/all-the-icons-completion")
       :init
       (all-the-icons-completion-mode)
       :config
       (with-eval-after-load "marginalia"
         (add-hook 'marginalia-mode-hook #'all-the-icons-marginalia-setup)))))

(provide 'init-foo)
