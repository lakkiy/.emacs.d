;;; -*- lexical-binding: t -*-

;; TODO customize face
(leaf tab-bar
  :doc "save window layout, gui not work on macos"
  :tag "builtin"
  :when (eq system-type 'gnu/linux)
  :commands
  (tab-bar-mode)
  ;; FIXME org mode use cc ct
  :bind (("C-c C-t t" . tab-bar-mode)
         ("C-c C-t r" . tab-bar-rename-tab)
         ("C-c C-t n" . tab-bar-new-tab)
         ("C-c C-t d" . tab-bar-close-tab))
  :custom
  (tab-bar-new-tab-choice . "*scratch*")
  (tab-bar-close-button-show . nil)
  (tab-bar-new-button-show . nil)
  :custom-face
  ;; TODO for joker theme
  (tab-bar . '((t (:inherit nil variable-pitch :backgronud "#131313" :foreground "#E0E0E0"))))
  (tab-bar-tab . '((t (:inherit nil :backgronud "#131313" :foreground "#E0E0E0"))))
  (tab-bar-tab-inactive . '((t (:inherit nil :backgronud "#131313")))))

(leaf hackernews
  :straight
  (hackernews :type git
              :host github
              :repo "clarete/hackernews.el")
  :commands
  (hackernews))

(leaf eaf
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
  (eaf-open-browser)
  :when (eq system-type 'gnu/linux)
  :config
  (eaf-setq eaf-browser-enable-adblocker "true"))

;; Web search
(leaf webjump
  :tag "builtin"
  ;; C-c / will be shadowed by `org-sparse-tree' in org-mode
  :bind ("C-c C-/" . webjump)
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

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(provide 'init-fun)
