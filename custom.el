(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-idle 3)
 '(aw-keys '(97 111 101 117 105) t)
 '(aw-scope 'frame t)
 '(company-dabbrev-downcase nil)
 '(company-global-modes '(not org-mode dired-mode dired-sidebar-mode))
 '(company-idle-delay 0.2)
 '(company-require-match nil)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-idle-delay 0.1)
 '(company-tooltip-limit 10)
 '(company-tooltip-width-grow-only t)
 '(custom-safe-themes
   '("21388667ce5ee0b375e6282f0d6c6b61588da6604d343bbb19389e6a54d3d00d" "602c523efed33ab7bc708c31ed9d641abe123e2bfbbe124e46faf68c95e67d8d" default))
 '(default-input-method "rime")
 '(dumb-jump-aggressive t t)
 '(dumb-jump-disable-obsolete-warnings t t)
 '(dumb-jump-prefer-searcher 'rg t)
 '(dumb-jump-quiet t t)
 '(dumb-jump-selector 'ivy t)
 '(eglot-ignored-server-capabilites '(:documentHighlightProvider))
 '(eglot-stay-out-of nil t)
 '(ibuffer-vc-skip-if-remote nil t)
 '(ivy-count-format "%d/%d ")
 '(ivy-use-selectable-prompt t)
 '(markdown-fontify-code-blocks-natively t t)
 '(mini-frame-show-parameters '((top . 10) (width \.0\.7) (left \.0\.5)) t)
 '(python-indent-offset 4 t)
 '(python-shell-interpreter "python3" t)
 '(rime-cursor "|")
 '(rime-disable-predicates '(meow-normal-mode-p meow-motion-mode-p meow-keypad-mode-p))
 '(rime-show-candidate 'minibuffer)
 '(rime-title "rime" t)
 '(rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g") t)
 '(rime-user-data-dir "~/.config/fcitx/rime")
 '(tab-bar-close-button-show nil)
 '(tab-bar-new-button-show nil)
 '(tab-bar-new-tab-choice "*scratch*")
 '(tab-bar-show 1)
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
 '(aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))) nil "Customized with leaf in `ace-window' block at `/home/liubo/.config/emacs/lisp/init-window.el'")
 '(tab-bar ((t (:inherit mode-line))) nil "Customized with leaf in `tab-bar' block at `/home/liubo/.config/emacs/lisp/init-window.el'")
 '(tab-bar-tab ((t (:inherit mode-line))) nil "Customized with leaf in `tab-bar' block at `/home/liubo/.config/emacs/lisp/init-window.el'")
 '(tab-bar-tab-inactive ((t (:inherit mode-line-inactive))) nil "Customized with leaf in `tab-bar' block at `/home/liubo/.config/emacs/lisp/init-window.el'"))
