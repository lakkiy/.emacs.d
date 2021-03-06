(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-idle 3)
 '(aw-keys '(97 111 101 117 105))
 '(aw-scope 'frame)
 '(company-dabbrev-downcase nil)
 '(company-global-modes '(not org-mode dired-mode dired-sidebar-mode))
 '(company-idle-delay 0.2)
 '(company-require-match nil)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-idle-delay 0.1)
 '(company-tooltip-limit 10)
 '(company-tooltip-width-grow-only t)
 '(custom-safe-themes
   '("68ef0fa46ad046d373ef58cba947e355c778542e74bdeb01ee345e7453420bd5" default))
 '(dumb-jump-aggressive t t)
 '(dumb-jump-disable-obsolete-warnings t t)
 '(dumb-jump-prefer-searcher 'rg t)
 '(dumb-jump-quiet t t)
 '(dumb-jump-selector 'ivy t)
 '(eglot-ignored-server-capabilites '(:documentHighlightProvider))
 '(eglot-stay-out-of nil t)
 '(elpy-modules
   '(elpy-module-company elpy-module-folding elpy-module-yasnippet) t)
 '(elpy-rpc-virtualenv-path 'current t)
 '(ivy-count-format "%d/%d ")
 '(ivy-use-selectable-prompt t)
 '(markdown-fontify-code-blocks-natively t)
 '(mini-frame-show-parameters '((top . 10) (width \.0\.7) (left \.0\.5)) t)
 '(newsticker-automatically-mark-items-as-old nil t)
 '(newsticker-automatically-mark-visited-items-as-old t t)
 '(newsticker-download-logos nil t)
 '(newsticker-enable-logo-manipulations nil t)
 '(newsticker-keep-obsolete-items t t)
 '(newsticker-obsolete-item-max-age 2592000 t)
 '(newsticker-retrieval-method 'extern t)
 '(newsticker-treeview-date-format "%F %a, %H:%M  " t)
 '(newsticker-treeview-listwindow-height 20 t)
 '(newsticker-url-list
   '(("Planet Emacslife" "https://planet.emacslife.com/atom.xml")
     ("Mastering Emacs" "http://www.masteringemacs.org/feed/")
     ("Oremacs" "https://oremacs.com/atom.xml")
     ("EmacsCast" "https://pinecast.com/feed/emacscast")
     ("LWN" "https://lwn.net/headlines/rss")) t)
 '(newsticker-url-list-defaults nil t)
 '(org-html-themify-themes '((dark \.joker) (light . storybook)) t)
 '(org-roam-directory "/home/liubo/Dropbox/org")
 '(tab-bar-close-button-show nil)
 '(tab-bar-new-button-show nil)
 '(tab-bar-new-tab-choice "*scratch*")
 '(treemacs-no-png-images t t)
 '(treemacs-width 30 t)
 '(webjump-sites
   '(("Google" .
      [simple-query "www.google.com" "www.google.com/search?q=" ""])
     ("Wikipedia" .
      [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])
     ("Urban Dictionary" .
      [simple-query "urbandictionary.com" "www.urbandictionary.com/define.php?term=" ""])
     ("Ludwig Guru" .
      [simple-query "ludwig.guru" "ludwig.guru/s/" ""])
     ("Etymology" .
      [simple-query "etymonline.com" "etymonline.com/word/" ""])
     ("Stack Overflow" .
      [simple-query "stackoverflow.com" "stackoverflow.com/search?q=" ""])
     ("TLDR" .
      [simple-query "linux.cn" "tldr.linux.cn/cmd/" ""])
     ("Man Search" .
      [simple-query "archlinux.org" "man.archlinux.org/search?q=" ""])
     ("Man Go" .
      [simple-query "archlinux.org" "man.archlinux.org/search?q=" "&go=Go"])
     ("x86 Instructions Reference" .
      [simple-query "www.felixcloutier.com" "www.felixcloutier.com/x86/" ""])
     ("Python Docs" .
      [simple-query "docs.python.org" "docs.python.org/3/search.html?q=" ""])
     ("Cpp Reference" .
      [simple-query "en.cppreference.com" "en.cppreference.com/mwiki/index.php?search=" ""])) t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 3.0))) nil "Customized with leaf in `ace-window' block at `/home/liubo/.config/emacs/lisp/init-window.el'")
 '(aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 2.0))) nil "Customized with leaf in `ace-window' block at `/home/liubo/.config/emacs/lisp/init-window.el'")
 '(aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))) nil "Customized with leaf in `ace-window' block at `/home/liubo/.config/emacs/lisp/init-window.el'"))
