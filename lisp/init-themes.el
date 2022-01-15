;;; -*- lexical-binding: t -*-
;; Do not use `eat-package' with themes.

;; `spacemacs-theme'
(straight-use-package 'spacemacs-theme)

(setq
 spacemacs-theme-comment-italic t
 spacemacs-theme-keyword-italic t
 spacemacs-theme-org-agenda-height t
 spacemacs-theme-org-bold t
 spacemacs-theme-org-height t
 spacemacs-theme-org-highlight t
 spacemacs-theme-org-priority-bold t
 spacemacs-theme-org-bold t
 spacemacs-theme-underline-parens t)

;; `kaolin-themes'
(straight-use-package 'kaolin-themes)

(setq
 kaolin-themes-underline-wave nil
 kaolin-themes-modeline-border nil
 kaolin-themes-modeline-padded 4)

(with-eval-after-load 'kaolin-themes
  ;; NOTE maybe check `+icons-p' and `all-the-icons'
  (with-eval-after-load 'treemacs
    (with-eval-after-load 'all-the-icons
      (kaolin-treemacs-theme))))

;; `doom-themes'
(straight-use-package 'doom-themes)

(with-eval-after-load 'doom-themes
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; `the-matrix-theme'
(straight-use-package 'the-matrix-theme)

;; FIXME not work on emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (unless +theme-system-appearance
              (load-theme +theme t))))

(provide 'init-themes)
