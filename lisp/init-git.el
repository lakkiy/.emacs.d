;;; -*- lexical-binding: t -*-

;;; ediff
(defvar local-ediff-saved-window-conf nil)
(defun eat/ediff-save-window-conf ()
  (setq local-ediff-saved-window-conf (current-window-configuration)))

(defun eat/ediff-restore-window-conf ()
  (when (window-configuration-p local-ediff-saved-window-conf)
    (set-window-configuration local-ediff-saved-window-conf)))

(setq ediff-window-setup-function #'ediff-setup-windows-plain
      ediff-highlight-all-diffs t
      ediff-split-window-function 'split-window-horizontally
      ediff-merge-split-window-function 'split-window-horizontally)
(with-eval-after-load 'ediff
  ;; Restore window config after quitting ediff
  (add-hook 'ediff-before-setup-hook #'eat/ediff-save-window-conf)
  (add-hook 'ediff-quit-hook #'eat/ediff-restore-window-conf))

;;; smerge
(add-hook 'find-file-hook #'(lambda ()
                              (save-excursion
                                (goto-char (point-min))
                                (when (re-search-forward "^<<<<<<< " nil t)
                                  (smerge-mode 1)))))

(with-eval-after-load 'smerge-mode
  (keymap-set smerge-mode-map "C-c c" #'smerge-keep-current)
  (keymap-set smerge-mode-map "C-c a" #'smerge-smerge-keep-all)
  (keymap-set smerge-mode-map "M-r" #'smerge-refine)
  (keymap-set smerge-mode-map "M-n" #'smerge-next)
  (keymap-set smerge-mode-map "M-p" #'smerge-prev))

;;; magit
(install-package 'magit)
(keymap-global-set "C-x g" #'magit-status)

(install-package 'magit-delta)

(when (executable-find "delta")
  (add-hook 'magit-mode-hook #'magit-delta-mode))

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

(provide 'init-git)
