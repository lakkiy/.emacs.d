;;; -*- lexical-binding: t -*-

(eat-package recentf
  ;; FIXME
  :hook (on-first-file-hook . recentf-mode)
  :init
  (setq
   recentf-max-saved-items 1000
   recentf-exclude `(,tramp-file-name-regexp
                     "COMMIT_EDITMSG"))
  (global-set-key (kbd "C-x C-r") #'recentf-open-files))

(eat-package so-long
  ;; FIXME
  :hook (on-first-buffer-hook . global-so-long-mode))

(eat-package autorevert
  ;; FIXME
  :hook (on-first-buffer-hook . global-auto-revert-mode))

(eat-package saveplace
  ;; FIXME
  :hook (on-first-buffer-hook . save-place-mode))

(eat-package winner
  :hook (on-init-ui-hook . winner-mode)
  :init
  (setq winner-dont-bind-my-keys t))

(eat-package desktop
  :hook (desktop-after-read-hook . eat/desktop-load-theme)
  :init
  (setq desktop-path (list user-emacs-directory)
        desktop-auto-save-timeout 600)
  (desktop-save-mode 1)

  ;; Reload theme after `desktop-read'.
  ;; But it doesn't prevent the desktop-save-mode from saving the theme
  ;; in the .desktop file, instead it restores the theme after loading
  ;; the desktop.
  (defun eat/desktop-load-theme ()
    "load custom theme"
    (interactive)
    (eat/load-theme (car custom-enabled-themes)))

  ;; save a bunch of variables to the desktop file
  ;; for lists specify the len of the maximal saved data also
  (setq desktop-globals-to-save
        '((comint-input-ring        . 50)
          (compile-history          . 30)
          desktop-missing-file-warning
          custom-enabled-themes
          (dired-regexp-history     . 20)
          (extended-command-history . 30)
          (face-name-history        . 20)
          (file-name-history        . 100)
          (grep-find-history        . 30)
          (grep-history             . 30)
          (magit-revision-history   . 50)
          (minibuffer-history       . 50)
          (org-clock-history        . 50)
          (org-refile-history       . 50)
          (org-tags-history         . 50)
          (query-replace-history    . 60)
          (read-expression-history  . 60)
          (regexp-history           . 60)
          (regexp-search-ring       . 20)
          register-alist
          (search-ring              . 20)
          (shell-command-history    . 50)
          tags-file-name
          tags-table-list)))


(eat-package savehist
  :hook (on-init-ui-hook . savehist-mode)
  :init
  ;; Restore histories and registers after saving
  (setq history-length 1000))

(eat-package fullframe :straight t)

(eat-package goto-addr
  :hook (on-init-ui-hook . global-goto-address-mode))

(defun term-mode-common-init ()
  "The common initialization procedure for term/shell."
  (setq-local scroll-margin 0)
  (setq-local truncate-lines t)
  (setq-local global-hl-line-mode nil))

(eat-package term
  :hook
  (term-mode-hook . (term-mode-prompt-regexp-setup term-mode-common-init))
  :init
  (defun term-mode-prompt-regexp-setup ()
    "Setup `term-prompt-regexp' for term-mode."
    (setq-local term-prompt-regexp "^[^#$%>\n]*[#$%>] *")))

(eat-package eshell
  :hook
  (eshell-mode-hook . (lambda ()
                        (term-mode-common-init)
                        ;; Eshell is not fully functional
                        (setenv "PAGER" "cat")))
  :config
  ;; Prevent accident typing
  (defalias 'eshell/vi 'find-file)
  (defalias 'eshell/vim 'find-file)

  (defun eshell/bat (file)
    "cat FILE with syntax highlight."
    (with-temp-buffer
      (insert-file-contents file)
      (let ((buffer-file-name file))
        (delay-mode-hooks
          (set-auto-mode)
          (font-lock-ensure)))
      (buffer-string)))

  (defun eshell/f (filename &optional dir)
    "Search for files matching FILENAME in either DIR or the
current directory."
    (let ((cmd (concat
                (executable-find "find")
                " " (or dir ".")
                "      -not -path '*/.git*'"
                " -and -not -path 'build'"    ;; the cmake build directory
                " -and"
                " -type f"
                " -and"
                " -iname '*" filename "*'")))
      (eshell-command-result cmd)))

  (defun eshell/z ()
    "cd to directory with completion."
    (let ((dir (completing-read "Directory: " (ring-elements eshell-last-dir-ring) nil t)))
      (eshell/cd dir)))

  (defun eshell-prompt ()
    "Prompt for eshell."
    (concat
     (propertize user-login-name 'face 'font-lock-keyword-face)
     "@"
     "Noobmaster "
     (if (equal (eshell/pwd) "~")
         "~"
       (abbreviate-file-name (eshell/pwd)))
     " "
     (if-let* ((vc (ignore-errors (vc-responsible-backend default-directory)))
               (br (car (vc-git-branches))))
         (concat (propertize "(" 'face 'success)
                 (format "%s" vc)
                 (propertize ")" 'face 'success)
                 (propertize "-" 'face 'font-lock-string-face)
                 (propertize "[" 'face 'success)
                 (propertize br 'face 'font-lock-constant-face)
                 (propertize "]" 'face 'success)
                 " ")
       "")
     "% ")))

;; The interactive shell.
;;
;; It can be used as a `sh-mode' REPL.
;;
;; `shell' is recommended to use over `tramp'.
(eat-package shell
  :hook
  (shell-mode-hook . (term-mode-common-init revert-tab-width-to-default))
  :init
  (defun shell-toggle ()
    "Toggle a persistent shell popup window.
If popup is visible but unselected, select it.
If popup is focused, kill it."
    (interactive)
    (if-let ((win (get-buffer-window "*shell-popup*")))
        (when (eq (selected-window) win)
          ;; If users attempt to delete the sole ordinary window, silence it.
          (ignore-errors (delete-window win)))
      (let ((display-comint-buffer-action '(display-buffer-at-bottom
                                            (inhibit-same-window . nil))))

        (shell "*shell-popup*"))))

  ;; Correct indentation for `ls'
  (defun revert-tab-width-to-default ()
    "Revert `tab-width' to default value."
    (setq-local tab-width 8)))

;;; init-builtin.el ends here
(provide 'init-builtin)
