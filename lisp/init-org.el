;;; -*- lexical-binding: t -*-

(leaf easy-hugo
  :straight t
  :commands
  (easy-hugo)
  :pre-setq
  ;; Set easy-hugo-server-flags to -D in order to preview drafts.
  (easy-hugo-server-flags . "-D")
  (easy-hugo-basedir . "~/bookshelf/")
  (easy-hugo-previewtime . "300")
  (easy-hugo-default-ext . ".org")
  (easy-hugo-org-header . t))

(leaf valigh
  :straight
  (valign :type git :host github :repo "casouri/valign")
  :config
  (add-hook 'org-mode-hook #'valign-mode))

(leaf org :tag "builtin")

(leaf org-superstar
  :straight t
  :init
  (setq org-superstar-leading-bullet ?\s)
  :hook (org-mode-hook . org-superstar-mode))

(leaf org-html-themify
  :straight
  (org-html-themify :type git
                    :host github
                    :repo "DogLooksGood/org-html-themify"
                    :files ("*.el" "*.js" "*.css"))
  :hook (org-mode-hook . org-html-themify-mode)
  :custom
  (org-html-themify-themes . '((dark .joker)
                               (light . storybook))))

(leaf org-roam
  :straight t
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
