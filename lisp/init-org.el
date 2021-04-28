;;; -*- lexical-binding: t -*-


(straight-use-package 'easy-hugo)

(setq
 easy-hugo-server-flags "-D"
 easy-hugo-basedir "~/bookshelf/"
 easy-hugo-previewtime "300"
 easy-hugo-default-ext ".org"
 easy-hugo-org-header t)

(autoload #'easy-hugo "easy-hugo" nil t)


(straight-use-package 'org-superstar)

(setq org-superstar-leading-bullet ?\s)

(autoload #'org-superstar-mode "org-superstar")

(add-hook 'org-mode-hook 'org-superstar-mode)

(with-eval-after-load "org-superstar"
  (setq org-superstar-headline-bullets-list
        '("☰"
          "☱"
          "☲"
          "☳"
          "☴"
          "☵"
          "☶"
          "☷")))


(straight-use-package '(org-html-themify
                        :type git
                        :host github
                        :repo "DogLooksGood/org-html-themify"
                        :files ("*.el " "*.js" "*.css")))

(setq
 org-html-themify-themes '((dark . lazycat-dark)
                           (light . lazycat-light)))

(autoload #'org-html-themify-mode "org-html-themify")

(add-hook 'org-mode-hook 'org-html-themify-mode)


(leaf org-roam
  :straight t
  :after org
  :hook (after-init-hook . org-roam-mode)
  :bind ((org-roam-mode-map
          ("C-c n l" . org-roam)
          ("C-c n f" . org-roam-find-file)
          ("C-c n g" . org-roam-graph))
         (org-mode-map
          ("C-c n i" . org-roam-insert)
          ("C-c n I" . org-roam-insert-immediate)))
  :custom
  `(org-roam-directory . ,(expand-file-name "~/Dropbox/org"))
  :require org-roam-protocol)

(leaf org-roam-server
  :straight t
  :commands
  (org-roam-server-mode)
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files t
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(provide 'init-org)
