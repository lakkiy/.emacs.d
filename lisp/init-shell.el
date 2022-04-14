;;; -*- lexical-binding: t -*-

;; The interactive shell.
;;
;; It can be used as a `sh-mode' REPL.
;;
;; `shell' is recommended to use over `tramp'.
(eat-package shell
  :hook
  (comint-mode-hook
   . (my-comint-init revert-tab-width-to-default))
  :init
  (defun my-comint-init ()
    (setq-local
     comint-input-ignoredups t
     comint-process-echoes t))

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

(eat-package eshell)

(eat-package term
  :init
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(eat-package vterm :straight t)
(eat-package vterm-toggle
  :straight t
  :init
  (global-set-key (kbd "C-`") #'vterm-toggle))

;;; init-shell.el ends here
(provide 'init-shell)
