(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline success warning error])
 '(ansi-color-names-vector
   ["#242525" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#00CE00"])
 '(auto-save-idle 3)
 '(aw-keys '(97 111 101 117 105))
 '(aw-scope 'frame)
 '(awesome-tray-mode-line-active-color "#2fafff")
 '(awesome-tray-mode-line-inactive-color "#323232")
 '(company-dabbrev-downcase nil)
 '(company-global-modes '(not org-mode dired-mode dired-sidebar-mode))
 '(company-idle-delay 0.2)
 '(company-require-match nil)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-idle-delay 0.1)
 '(company-tooltip-limit 10)
 '(company-tooltip-width-grow-only t)
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#689d6a")
 '(cua-normal-cursor-color "#7c6f64")
 '(cua-overwrite-cursor-color "#b57614")
 '(cua-read-only-cursor-color "#98971a")
 '(custom-safe-themes
   '("3f2744b33922de5f7990413e21bf9fc2411a91df8164ccac140f929c53ab8098" "ad3f5b2649fe389dd87a220742a4d6c08b156ecdf4988afbc1d4bc40dd17af02" "f0188aefc06b9df983151fa15d5d57dc9be0fb9c57b929c7d2726c849a7ee4f7" "ffbca34f30d6acbf8c03364eb5845bfdc6f8f6ae8ac91e8a9c8a942d536e11f3" "f70713b291c88312267bf85d79778d4f0dfe02bd3b9d06df2cb85332cd7e71b4" "1639830d682864fecc6d393237c1286b1a00310d965dc2eba27565f4945f55fe" "5cf967ebce04f73f40dfec2a2fd11476ade2541f1970eab883db838265d150f9" "d48ed54cac52d7d151aa2f3321dc1462a633dad8d2ec7c95b79bc21ad0e72fdf" "0662e3fbfe80821ee133053f9a68f87fa83331bc45add5f85e19633597d41ffc" "431adcc780980776e662d0427c72e7f763c2ab2c27125b4b1107d4fde981e266" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "c7f364aeea0458b6368815558cbf1f54bbdcc1dde8a14b5260eb82b76c0ffc7b" "0c860c4fe9df8cff6484c54d2ae263f19d935e4ff57019999edbda9c7eda50b8" "9c29b2670f8bcd5a999e09e74d8cae54c6e4b8d5197da3e0bb2faae6eb1550ca" "be65acc404f37158db42f7a2950583b65475371c34b17f9647db73944a7ee915" default))
 '(default-input-method "rime")
 '(dumb-jump-aggressive t t)
 '(dumb-jump-disable-obsolete-warnings t t)
 '(dumb-jump-prefer-searcher 'rg t)
 '(dumb-jump-quiet t t)
 '(dumb-jump-selector 'ivy t)
 '(eglot-ignored-server-capabilites '(:documentHighlightProvider))
 '(eglot-stay-out-of nil t)
 '(exwm-floating-border-color "#646464")
 '(fci-rule-color "#5B6268")
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-themes-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-themes-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-themes-fringe-yellow))
 '(highlight-changes-colors '("#d3869b" "#8f3f71"))
 '(highlight-symbol-colors
   '("#ed94d1d49b5c" "#d6a6dca4af86" "#eb91bc26933d" "#e1bec426b1e5" "#e40eda899de8" "#ef29c40f9556" "#c667cd43b3ba"))
 '(highlight-symbol-foreground-color "#665c54")
 '(highlight-tail-colors '(("#2f4a00" . 0) ("#00415e" . 20)))
 '(hl-bg-colors
   '("#e29a3f" "#df6835" "#cf5130" "#f598a7" "#c2608f" "#5b919b" "#82cc73" "#c6c148"))
 '(hl-fg-colors
   '("#fbf1c7" "#fbf1c7" "#fbf1c7" "#fbf1c7" "#fbf1c7" "#fbf1c7" "#fbf1c7" "#fbf1c7"))
 '(hl-paren-colors '("#689d6a" "#b57614" "#076678" "#8f3f71" "#98971a"))
 '(hl-todo-keyword-faces
   '(("HOLD" . "#cfdf30")
     ("TODO" . "#feacd0")
     ("NEXT" . "#b6a0ff")
     ("THEM" . "#f78fe7")
     ("PROG" . "#00d3d0")
     ("OKAY" . "#4ae8fc")
     ("DONT" . "#70c900")
     ("FAIL" . "#ff8059")
     ("BUG" . "#ff8059")
     ("DONE" . "#44bc44")
     ("NOTE" . "#f0ce43")
     ("KLUDGE" . "#eecc00")
     ("HACK" . "#eecc00")
     ("TEMP" . "#ffcccc")
     ("FIXME" . "#ff9977")
     ("XXX+" . "#f4923b")
     ("REVIEW" . "#6ae4b9")
     ("DEPRECATED" . "#bfd9ff")))
 '(ibuffer-deletion-face 'modus-themes-mark-del)
 '(ibuffer-filter-group-name-face 'modus-themes-mark-symbol)
 '(ibuffer-marked-face 'modus-themes-mark-sel)
 '(ibuffer-title-face 'modus-themes-pseudo-header)
 '(ibuffer-vc-skip-if-remote nil t)
 '(ivy-count-format "%d/%d ")
 '(ivy-use-selectable-prompt t)
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "green"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(lazycat-dark-brighter-modeline t)
 '(lsp-ui-doc-border "#665c54")
 '(markdown-fontify-code-blocks-natively t)
 '(nrepl-message-colors
   '("#9d0006" "#af3a03" "#b57614" "#747400" "#c6c148" "#004858" "#689d6a" "#d3869b" "#8f3f71"))
 '(objed-cursor-color "#ff6c6b")
 '(org-html-themify-themes '((dark \.joker) (light . storybook)) t)
 '(org-roam-directory "/home/liubo/Dropbox/org")
 '(org-src-block-faces 'nil)
 '(pdf-view-midnight-colors (cons "#00CE00" "#242525"))
 '(pos-tip-background-color "#ebdbb2")
 '(pos-tip-foreground-color "#665c54")
 '(python-indent-offset 4)
 '(python-shell-interpreter "python3")
 '(rime-cursor "|")
 '(rime-disable-predicates '(meow-normal-mode-p meow-motion-mode-p meow-keypad-mode-p))
 '(rime-show-candidate 'minibuffer)
 '(rime-title "rime" t)
 '(rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g") t)
 '(rime-user-data-dir "~/.config/fcitx/rime")
 '(rustic-ansi-faces
   ["#242525" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#00CE00"])
 '(smartrep-mode-line-active-bg (solarized-color-blend "#98971a" "#ebdbb2" 0.2))
 '(solaire-global-mode nil)
 '(tab-bar-close-button-show nil)
 '(tab-bar-new-button-show nil)
 '(tab-bar-new-tab-choice "*scratch*")
 '(tab-bar-show 1)
 '(term-default-bg-color "#fbf1c7")
 '(term-default-fg-color "#7c6f64")
 '(tetris-x-colors
   [[229 192 123]
    [97 175 239]
    [209 154 102]
    [224 108 117]
    [152 195 121]
    [198 120 221]
    [86 182 194]])
 '(treemacs-no-png-images t)
 '(treemacs-width 30)
 '(vc-annotate-background "#242525")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#e6ab6a")
    (cons 120 "#e09859")
    (cons 140 "#da8548")
    (cons 160 "#d38079")
    (cons 180 "#cc7cab")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#cf6162")
    (cons 300 "#9f585a")
    (cons 320 "#6f4e52")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
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
 '(weechat-color-list
   '(unspecified "#fbf1c7" "#ebdbb2" "#750000" "#9d0006" "#747400" "#98971a" "#8a5100" "#b57614" "#004858" "#076678" "#9f4d64" "#d3869b" "#2e7d33" "#689d6a" "#7c6f64" "#3c3836"))
 '(xterm-color-names
   ["black" "#ff8059" "#44bc44" "#eecc00" "#2fafff" "#feacd0" "#00d3d0" "gray65"])
 '(xterm-color-names-bright
   ["gray35" "#f4923b" "#70c900" "#cfdf30" "#79a8ff" "#f78fe7" "#4ae8fc" "white"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Operator Mono SSm Lig" :foundry "H&Co" :slant normal :weight light :height 113 :width normal))))
 '(aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 3.0))) nil "Customized with leaf in `ace-window' block at `/home/liubo/.config/emacs/lisp/init-window.el'")
 '(aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 2.0))) nil "Customized with leaf in `ace-window' block at `/home/liubo/.config/emacs/lisp/init-window.el'")
 '(aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))) nil "Customized with leaf in `ace-window' block at `/home/liubo/.config/emacs/lisp/init-window.el'")
 '(tab-bar ((t (:inherit mode-line))) nil "Customized with leaf in `tab-bar' block at `/home/liubo/.config/emacs/lisp/init-window.el'")
 '(tab-bar-tab ((t (:inherit mode-line-inactive))) nil "Customized with leaf in `tab-bar' block at `/home/liubo/.config/emacs/lisp/init-window.el'")
 '(tab-bar-tab-inactive ((t (:inherit mode-line))) nil "Customized with leaf in `tab-bar' block at `/home/liubo/.config/emacs/lisp/init-window.el'"))
