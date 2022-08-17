;;; -*- lexical-binding: t -*-

(eat-package recentf
  :hook (after-init-hook . recentf-mode)
  :init
  (setq
   recentf-max-saved-items 1000
   recentf-exclude `(,tramp-file-name-regexp
                     "COMMIT_EDITMSG"))
  (global-set-key (kbd "C-x C-r") #'recentf-open-files))

(eat-package so-long
  :hook (after-init-hook . global-so-long-mode))

(eat-package autorevert
  :hook (after-init-hook . global-auto-revert-mode))

(eat-package saveplace
  :hook (after-init-hook . save-place-mode))

(eat-package winner
  :hook (after-init-hook . winner-mode)
  :init
  (setq winner-dont-bind-my-keys t))

(eat-package savehist
  :hook (after-init-hook . savehist-mode)
  :init
  ;; Restore histories and registers after saving
  (setq history-length 1000))

(eat-package fullframe :straight t)

(eat-package goto-addr
  :hook (after-init-hook . global-goto-address-mode))

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
