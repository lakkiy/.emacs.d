;;; -*- lexical-binding: t -*-

(use-package valign
  :straight
  (valign :type git
          :host github
          :repo "casouri/valign")
  :config
  (add-hook 'org-mode-hook #'valign-mode))

(use-package org
  :straight (:type built-in))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

(use-package org-html-themify
  :straight
  (org-html-themify
   :type git
   :host github
   :repo "DogLooksGood/org-html-themify"
   :files ("*.el" "*.js" "*.css"))
  :hook (org-mode . org-html-themify-mode)
  :custom
  (org-html-themify-themes
   '((dark . joker)
     (light . storybook))))

(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory (expand-file-name "~/Dropbox/org"))
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate)))
  :config
  (require 'org-roam-protocol))

(use-package org-roam-server
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
