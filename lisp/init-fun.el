;;; -*- lexical-binding: t -*-

(leaf olivetti
  :doc "A simple Emacs minor mode for a nice writing environment."
  :straight t
  :commands
  (olivetti-mode))

(leaf vundo
  :tag "builtin"
  :init
  ;; (setq vundo-translation-alist
  ;;     '((?○ . ?●)))
  ;; (set-face-attribute 'vundo-highlight nil :foreground "red")
  ;; (set-face-attribute 'vundo-node nil :foreground "green")
  ;; (set-face-attribute 'vundo-stem nil :foreground "green")
  :commands
  (vundo))

(leaf mini-frame-mode
  :straight
  (mini-frame-mode :type git
                   :host github
                   :repo "muffinmad/emacs-mini-frame")
  :custom
  (mini-frame-show-parameters . '((top . 10)
                                  (width .0.7)
                                  (left .0.5)))
  :commands
  (mini-frame-mode))
  ;; :global-minor-mode
  ;; (mini-frame-mode))

(leaf esup
  :straight t
  :commands
  (esup))

(leaf vterm
  :straight t
  :commands
  (vterm))

(leaf leetcode
  :straight t
  :setq
  (leetcode-prefer-language . "golang")
  (leetcode-prefer-sql . "mysql")
  (leetcode-save-solutions . t)
  (leetcode-directory . "~/Dropbox/leetcode")
  :commands
  (leetcode))

(leaf restclient
  :straight t
  :commands
  (restclient-mode))

;; Web search
(leaf webjump
  :tag "builtin"
  ;; C-c / will be shadowed by `org-sparse-tree' in org-mode
  :bind ("C-c C-/" . webjump)
  :commands webjump
  :custom
  (webjump-sites . '(;; Internet search engines.
                     ("Google" .
                      [simple-query "www.google.com"
                                    "www.google.com/search?q=" ""])
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

                     ;; Language specific engines.
                     ("x86 Instructions Reference" .
                      [simple-query "www.felixcloutier.com"
                                    "www.felixcloutier.com/x86/" ""])
                     ("Python Docs" .
                      [simple-query "docs.python.org"
                                    "docs.python.org/3/search.html?q=" ""])
                     ("Cpp Reference" .
                      [simple-query "en.cppreference.com"
                                    "en.cppreference.com/mwiki/index.php?search=" ""]))))

(provide 'init-fun)
