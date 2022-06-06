;;; -*- lexical-binding: t -*-

(straight-use-package 'tao-theme)
(straight-use-package 'color-theme-sanityinc-tomorrow)
(straight-use-package 'doom-themes)
(straight-use-package 'spacemacs-theme)
(straight-use-package 'kaolin-themes)
(straight-use-package 'stimmung-themes)


;;; Theme
;; `doom-themes'
(with-eval-after-load 'doom-themes
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; `spacemacs-theme'
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
(setq
 kaolin-themes-underline-wave nil
 kaolin-themes-modeline-border nil
 kaolin-themes-modeline-padded 4)

(with-eval-after-load 'kaolin-themes
  (with-eval-after-load 'treemacs
    (with-eval-after-load 'all-the-icons
      (kaolin-treemacs-theme))))


;;; Mode-line

(progn
  (eat-package mode-line-bell :straight t)
  (eat-package which-func :commands which-func-mode)
  (eat-package minions :straight t)

  (defun luna-mode-line-with-padding (text)
    "Return TEXT with padding on the left.
The padding pushes TEXT to the right edge of the mode-line."
    (if (display-graphic-p)
        (let* ((len (string-pixel-width text))
               (space-prop
                `(space :align-to (- (+ right right-margin) (,len))))
               (padding (propertize "-" 'display space-prop)))
          (concat padding text))
      (concat " " text)))

  (defun luna-mode-line-coding-system ()
    "Display abnormal coding systems."
    (let ((coding (symbol-name buffer-file-coding-system)))
      (if (or (and (not (string-prefix-p "prefer-utf-8" coding))
                   (not (string-prefix-p "utf-8" coding))
                   (not (string-prefix-p "undecided" coding)))
              (string-suffix-p "dos" coding))
          (concat "  " coding)
        "")))

  (defun eat/setup-mode-line ()
    (mode-line-bell-mode)
    (which-func-mode)
    (minions-mode)
    (setq-default mode-line-format
                  (let* ((spaces
                          (propertize " " 'display '(space :width 1.5)))
                         (fringe (propertize
                                  " " 'display '(space :width fringe)))
                         (percentage
                          '(format
                            "[%%l] %d%%"
                            (/ (* (window-end) 100.0) (point-max)))))
                    `(,fringe
                      (:eval (when (fboundp 'meow-indicator) (meow-indicator)))
                      (:eval (when (fboundp 'rime-lighter) (rime-lighter)))
                      " "
                      (:eval (if (window-dedicated-p) "🚷" ""))
                      (:eval (if buffer-read-only "🔒" ""))
                      (:propertize "%[%b%]" face (:inherit mode-line-buffer-id :weight bold))
                      (:eval (luna-mode-line-coding-system))
                      ,spaces
                      ,(propertize " " 'display '(raise 0.3))
                      ,(if (featurep 'minions)
                           'minions-mode-line-modes
                         'mode-line-modes)
                      ,(propertize " " 'display '(raise -0.3))
                      (:eval (when (bound-and-true-p flymake-mode) (sekiro-flymake-mode-line-format)))
                      ,spaces
                      (:eval (if (buffer-modified-p)
                                 ,(if (display-graphic-p) "ΦAΦ" "OAO")
                               ,(if (display-graphic-p) "ΦωΦ" "OwO")))
                      ,spaces
                      mode-line-misc-info
                      (:eval (concat (luna-mode-line-with-padding ,percentage)
                                     "%%"))
                      )))))
(add-hook 'after-init-hook #'eat/setup-mode-line)

;;; init-ui.el ends here
(provide 'init-ui)
