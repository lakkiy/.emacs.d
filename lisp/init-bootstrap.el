;;; -*- lexical-binding: t -*-

;;; Bootstrap straight.el
;; https://www.reddit.com/r/emacs/comments/mtb05k/emacs_init_time_decreased_65_after_i_realized_the/
(setq straight-check-for-modifications '(check-on-save find-when-checking))
(setq straight-vc-git-default-clone-depth 1)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;;; Notifications
;;
;; Actually, `notify-send' is not defined in notifications package, but the
;; autoload cookie will make Emacs load `notifications' first, then our
;; `defalias' will be evaluated.
(pcase system-type
  ('gnu/linux
   (autoload #'notify-send "notifications")
   (with-eval-after-load "notifications"
     (defalias 'notify-send 'notifications-notify)))
  ('darwin
   ;; HACK you must enable notify for emacs in macos system
   ;;      Notifications & Focus -> Emacs -> Allow Notifications
   (defun notify-send (&rest params)
     "Send notifications via `terminal-notifier'."
     (let ((title (plist-get params :title))
           (body (plist-get params :body)))
       (start-process "terminal-notifier"
                      nil
                      "terminal-notifier"
                      "-group" "Emacs"
                      "-title" title
                      "-message" body
                      ;; FIXME this option didn't show emacs icon
                      ;; but -sender didn't show the message when focus on emacs
                      "-activate" "org.gnu.Emacs"))))
  (_
   (defalias 'notify-send 'ignore)))


;;; built-in packages
(eat-package recentf
  :hook (after-init-hook . recentf-mode)
  :init
  (setq
   recentf-max-saved-items 1000
   recentf-exclude `(,tramp-file-name-regexp
                     "COMMIT_EDITMSG"))
  (global-set-key (kbd "C-x C-r") #'recentf-open-files))

(eat-package goto-addr
  :hook (after-init-hook . global-goto-address-mode))


;;; site-lisp
(eat-package form-feed
  :hook
  ((emacs-lisp-mode-hook text-mode-hook special-mode-hook)
   . form-feed-mode))

(eat-package dired-toggle-sudo
  :commands
  dired-toggle-sudo)



(eat-package gcmh
  :straight t
  :hook (after-init-hook . gcmh-mode)
  :init
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold #x6400000)) ;; 100 MB

(eat-package benchmark-init
  :straight t
  :init
  (when eat/enable-benchmark
    (benchmark-init/activate))
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(eat-package default-text-scale
  :straight t
  :init
  (global-set-key (kbd "C-x C-=") #'default-text-scale-increase)
  (global-set-key (kbd "C-x C--") #'default-text-scale-decrease))

(eat-package all-the-icons :straight t)
(eat-package async :straight t)

;; 拼音搜索
(eat-package pinyinlib
  :straight t
  :commands
  pinyinlib-build-regexp-char
  pinyinlib-build-regexp-string)

(eat-package fullframe :straight t)

;;; init-bootstrap.el ends here
(provide 'init-bootstrap)
