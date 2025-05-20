;;; init-git.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;;; magit
(install-package 'magit)
(keymap-global-set "C-x g" #'magit-status)

(setq magit-diff-refine-hunk t
      magit-diff-paint-whitespace nil
      magit-format-file-function #'magit-format-file-nerd-icons)

(autoload #'gptel-commit "gptel-commit.el" nil t)

;;; diff-hl
(install-package 'diff-hl)

(defun enable-diff-hl-dired-locally ()
  (if (file-remote-p default-directory)
      (diff-hl-dired-mode -1)
    (diff-hl-dired-mode 1)))

(add-hook 'prog-mode-hook #'diff-hl-mode)
(add-hook 'conf-mode-hook #'diff-hl-mode)
(add-hook 'dired-mode-hook #'enable-diff-hl-dired-locally)

(setq diff-hl-draw-borders nil
      diff-hl-disable-on-remote t)

(with-eval-after-load 'diff-hl
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)
  (unless (display-graphic-p)
    ;; Fall back to the display margin since the fringe is unavailable in tty
    (diff-hl-margin-mode 1)
    ;; Avoid restoring `diff-hl-margin-mode'
    (with-eval-after-load 'desktop
      (add-to-list 'desktop-minor-mode-table
                   '(diff-hl-margin-mode nil)))))

;;; init-git.el ends here
