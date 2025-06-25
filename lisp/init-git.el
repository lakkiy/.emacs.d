;;; init-git.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;;; magit
(install-package 'magit)
(keymap-global-set "C-x g" #'magit-status)

(setq magit-diff-refine-hunk t
      magit-diff-paint-whitespace nil
      magit-format-file-function #'magit-format-file-nerd-icons)

;; optimize tramp
;;
;; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
(setq magit-tramp-pipe-stty-settings 'pty)

;; don't show the diff by default in the commit buffer. Use `C-c C-d' to display it
(setq magit-commit-show-diff nil)

;; don't show git variables in magit branch
(setq magit-branch-direct-configure nil)

;; don't automatically refresh the status buffer after running a git command
(setq magit-refresh-status-buffer nil)

;; Memoize magit top level
(defvar magit-toplevel-cache nil)
(defun memoize-magit-toplevel (orig &optional directory)
  (memoize-remote (or directory default-directory)
                  'magit-toplevel-cache orig directory))
(with-eval-after-load 'magit
  (advice-add 'magit-toplevel :around #'memoize-magit-toplevel))

;;; gptel-commit
(install-package 'gptel-commit "https://github.com/lakkiy/gptel-commit")

(with-eval-after-load 'magit
  (define-key git-commit-mode-map (kbd "C-c g") #'gptel-commit)
  (define-key git-commit-mode-map (kbd "C-c G") #'gptel-commit-rationale))

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
