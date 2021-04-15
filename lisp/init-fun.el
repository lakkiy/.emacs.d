;;; -*- lexical-binding: t -*-

(leaf ibuffer-vc
  :straight t
  :doc "group ibuffer"
  :added "2021-03-18"
  :commands (ibuffer-vc-set-filter-groups-by-vc-root)
  :custom
  (ibuffer-vc-skip-if-remote . 'nil))

(leaf olivetti
  :doc "A simple Emacs minor mode for a nice writing environment."
  :straight t
  :commands
  (olivetti-mode))

(leaf rainbow-mode
  :doc "show ansi-color, enable manually"
  :straight t
  :commands
  (rainbow-mode))

;; FIXME didn't work
(leaf docstr
  :straight t
  :added "2021-03-24"
  :doc "add doc string to func"
  :hook
  (prog-mode-hook . (lambda () (docstr-mode 1))))

(leaf parrot
  :straight t
  :init
  (setq parrot-rotate-dict
      '(
        (:rot ("alpha" "beta") :caps t :lower nil)
        ;; => rotations are "Alpha" "Beta"

        (:rot ("snek" "snake" "stawp"))
        ;; => rotations are "snek" "snake" "stawp"

        (:rot ("yes" "no") :caps t :upcase t)
        ;; => rotations are "yes" "no", "Yes" "No", "YES" "NO"

        (:rot ("&" "|"))
        ;; => rotations are "&" "|"

        ;; default dictionary starts here ('v')
        (:rot ("begin" "end") :caps t :upcase t)
        (:rot ("enable" "disable") :caps t :upcase t)
        (:rot ("enter" "exit") :caps t :upcase t)
        (:rot ("forward" "backward") :caps t :upcase t)
        (:rot ("front" "rear" "back") :caps t :upcase t)
        (:rot ("get" "set") :caps t :upcase t)
        (:rot ("high" "low") :caps t :upcase t)
        (:rot ("in" "out") :caps t :upcase t)
        (:rot ("left" "right") :caps t :upcase t)
        (:rot ("min" "max") :caps t :upcase t)
        (:rot ("on" "off") :caps t :upcase t)
        (:rot ("prev" "next"))
        (:rot ("start" "stop") :caps t :upcase t)
        (:rot ("true" "false") :caps t :upcase t)
        (:rot ("&&" "||"))
        (:rot ("==" "!="))
        (:rot ("." "->"))
        (:rot ("if" "else" "elif"))
        (:rot ("ifdef" "ifndef"))
        (:rot ("int8_t" "int16_t" "int32_t" "int64_t"))
        (:rot ("uint8_t" "uint16_t" "uint32_t" "uint64_t"))
        (:rot ("1" "2" "3" "4" "5" "6" "7" "8" "9" "10"))
        (:rot ("1st" "2nd" "3rd" "4th" "5th" "6th" "7th" "8th" "9th" "10th"))
        ))
  :config
  (parrot-mode))

(leaf eaf
  :doc "monkeytype in company (, don't forget run npm install"
  :straight
  (eaf :type git
       :host github
       :repo "manateelazycat/emacs-application-framework"
       :files ("*"))
  :init
  (leaf epc :straight t :leaf-defer t)
  (leaf ctable :straight t :leaf-defer t)
  (leaf deferred :straight t :leaf-defer t)
  (leaf s :straight t :leaf-defer t)
  :commands
  (eaf-open-browser eaf-open eaf-open-bookmark)
  :config
  (require 'eaf-org)
  (eaf-setq eaf-browser-enable-adblocker "true")
  (eaf-setq eaf-browser-enable-autofill "true"))

(leaf vundo
  :straight
  (vundo :type git
         :host github
         :repo "casouri/vundo")
  :added "2021-03-22"
  :doc "visual undo tree"
  :init
  ;; (setq vundo-translation-alist
  ;;     '((?○ . ?●)))
  ;; (set-face-attribute 'vundo-highlight nil :foreground "red")
  ;; (set-face-attribute 'vundo-node nil :foreground "green")
  ;; (set-face-attribute 'vundo-stem nil :foreground "green")
  :commands
  (vundo))

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
