;;; -*- lexical-binding: t -*-

(eat-package ibuffer-project
  :straight t
  :init
  ;; use `ibuffer-project-clear-cache' to clear cache
  (setq ibuffer-project-use-cache t)
  (custom-set-variables
   '(ibuffer-formats
     '((mark modified read-only locked " "
             (name 18 18 :left :elide)
             " "
             (size 9 -1 :right)
             " "
             (mode 16 16 :left :elide)
             " " project-file-relative))))
  (add-hook 'ibuffer-hook
            (lambda ()
              (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
              (unless (eq ibuffer-sorting-mode 'project-file-relative)
                (ibuffer-do-sort-by-project-file-relative))))
  :config
  ;; In this case all remote buffers will be grouped by a string identifying the remote connection.
  (add-to-list 'ibuffer-project-root-functions '(file-remote-p . "Remote")))

(provide 'init-ibuffer)
