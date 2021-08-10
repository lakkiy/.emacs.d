;;; -*- lexical-binding: t -*-

;; DOOM core/core-packages.el#L87-L90
;; https://www.reddit.com/r/emacs/comments/mtb05k/emacs_init_time_decreased_65_after_i_realized_the/
(setq straight-check-for-modifications '(check-on-save find-when-checking))
(setq straight-vc-git-default-clone-depth 1)

(setq straight-disable-native-compile
      (when (fboundp 'native-comp-available-p)
	(not (native-comp-available-p))))

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

(defun +set-github-mirror (oldfunc &rest args)
   (let ((url (apply oldfunc args)))
      (replace-regexp-in-string (rx (group "github.com"))
                                "github.com.cnpmjs.org" url nil nil 1)))
(advice-add 'straight-vc-git--encode-url :around #'+set-github-mirror)

(require 'eat-package)

(eat-package gcmh
  :straight t
  :hook (after-init-hook . gcmh-mode)
  :init
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold #x6400000)) ;; 100 MB

(provide 'init-straight)
