;;; -*- lexical-binding: t -*-
;; config should work under emacs -Q like straight and gc...

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

(eat-package on
  :straight (on :type git :host github :repo "ajgrf/on.el")
  :init
  (require 'on))

(defvar eat/enable-benchmark nil
  "Enable `benchmark-init', run `benchmark-init/show-durations-tree' to see result.")

(defvar eat/enable-icon t
  "Whether to enable `all-the-icons'.")


;;; GC
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

;;; init-default.el ends here
(provide 'init-default)
