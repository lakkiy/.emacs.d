(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline success warning error])
 '(ansi-color-names-vector
   ["#d2ceda" "#f2241f" "#67b11d" "#b1951d" "#3a81c3" "#a31db1" "#21b8c7" "#655370"])
 '(auto-save-idle 3)
 '(aw-keys '(97 111 101 117 105) t)
 '(aw-scope 'frame t)
 '(awesome-tray-mode-line-active-color "#2fafff")
 '(awesome-tray-mode-line-inactive-color "#323232")
 '(browse-url-browser-function 'eaf-open-browser)
 '(company-dabbrev-downcase nil)
 '(company-global-modes '(not org-mode dired-mode dired-sidebar-mode))
 '(company-idle-delay 0.2)
 '(company-require-match nil)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-idle-delay 0.1)
 '(company-tooltip-limit 10)
 '(company-tooltip-width-grow-only t)
 '(custom-safe-themes
   '("431adcc780980776e662d0427c72e7f763c2ab2c27125b4b1107d4fde981e266" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "81c3de64d684e23455236abde277cda4b66509ef2c28f66e059aa925b8b12534" "b1147c757ad892085f1bacef35164e87a2a7f4bd08b00b5f4577a75c6a9d563b" "602c523efed33ab7bc708c31ed9d641abe123e2bfbbe124e46faf68c95e67d8d" default))
 '(default-input-method "rime")
 '(dumb-jump-aggressive t t)
 '(dumb-jump-disable-obsolete-warnings t t)
 '(dumb-jump-prefer-searcher 'rg t)
 '(dumb-jump-quiet t t)
 '(dumb-jump-selector 'ivy t)
 '(eaf-browser-continue-where-left-off t)
 '(eglot-ignored-server-capabilites '(:documentHighlightProvider))
 '(eglot-stay-out-of nil t)
 '(exwm-floating-border-color "#646464")
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-theme-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-theme-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-theme-fringe-yellow))
 '(highlight-tail-colors '(("#2f4a00" . 0) ("#00415e" . 20)))
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#3a81c3")
     ("OKAY" . "#3a81c3")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#42ae2c")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(ibuffer-deletion-face 'modus-theme-mark-del)
 '(ibuffer-filter-group-name-face 'modus-theme-mark-symbol)
 '(ibuffer-marked-face 'modus-theme-mark-sel)
 '(ibuffer-title-face 'modus-theme-pseudo-header)
 '(ibuffer-vc-skip-if-remote nil t)
 '(ivy-count-format "%d/%d ")
 '(ivy-use-selectable-prompt t)
 '(markdown-fontify-code-blocks-natively t t)
 '(mini-frame-show-parameters '((top . 10) (width \.0\.7) (left \.0\.5)) t)
 '(org-html-themify-themes '((dark \.joker) (light . storybook)) t)
 '(org-roam-directory "/Users/liubo/Dropbox/org")
 '(org-src-block-faces 'nil)
 '(pdf-view-midnight-colors '("#655370" . "#fbf8ef"))
 '(python-indent-offset 4 t)
 '(python-shell-interpreter "python3" t)
 '(rime-cursor "|")
 '(rime-disable-predicates '(meow-normal-mode-p meow-motion-mode-p meow-keypad-mode-p))
 '(rime-show-candidate 'minibuffer)
 '(rime-title "rime" t)
 '(rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g") t)
 '(rime-user-data-dir "~/Library/Rime")
 '(treemacs-no-png-images t t)
 '(treemacs-width 30 t)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#ff8059")
     (40 . "#feacd0")
     (60 . "#f78fe7")
     (80 . "#f4923b")
     (100 . "#eecc00")
     (120 . "#cfdf30")
     (140 . "#f8dec0")
     (160 . "#bfebe0")
     (180 . "#44bc44")
     (200 . "#70c900")
     (220 . "#6ae4b9")
     (240 . "#4ae8fc")
     (260 . "#00d3d0")
     (280 . "#c6eaff")
     (300 . "#2fafff")
     (320 . "#79a8ff")
     (340 . "#00bcff")
     (360 . "#b6a0ff")))
 '(vc-annotate-very-old-color nil)
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
      [simple-query "en.cppreference.com" "en.cppreference.com/mwiki/index.php?search=" ""])) t)
 '(xterm-color-names
   ["black" "#ff8059" "#44bc44" "#eecc00" "#2fafff" "#feacd0" "#00d3d0" "gray65"])
 '(xterm-color-names-bright
   ["gray35" "#f4923b" "#70c900" "#cfdf30" "#79a8ff" "#f78fe7" "#4ae8fc" "white"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 3.0))) nil "Customized with leaf in `ace-window' block at `/Users/liubo/.config/emacs/lisp/init-window.el'")
 '(aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 2.0))) nil "Customized with leaf in `ace-window' block at `/Users/liubo/.config/emacs/lisp/init-window.el'")
 '(aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))) nil "Customized with leaf in `ace-window' block at `/Users/liubo/.config/emacs/lisp/init-window.el'"))
