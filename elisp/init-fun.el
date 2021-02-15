;;; -*- lexical-binding: t -*-

;; (use-package hackernews
;;   :straight
;;   (hackernews :type git
;;               :host github
;;               :repo "clarete/hackernews.el"))
(leaf hackernews
  :straight (hackernews :type git :host github :repo "clarete/hackernews.el"))

;; Web search
(use-package webjump
  :ensure nil
  ;; C-c / will be shadowed by `org-sparse-tree' in org-mode
  :bind ("C-c C-/" . webjump)
  :custom
  (webjump-sites '(;; Internet search engines.
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
