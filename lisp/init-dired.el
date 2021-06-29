;;; -*- lexical-binding: t -*-

(eat-package dired-hacks-utils
  :straight t
  :init
  (pretty-hydra-define dired-hydra (:title "Dired" :quit-key "q")
    ("TODO One"
     (("(" dired-hide-details-mode)
      (")" dired-omit-mode)
      ("+" dired-create-directory)
      ("=" diredp-ediff)         ;; smart diff
      ("?" dired-summary)
      ("$" diredp-hide-subdir-nomove)
      ("A" dired-do-find-regexp)
      ("C" dired-do-copy)        ;; Copy all marked files
      ("D" dired-do-delete)
      ("E" dired-mark-extension)
      ("e" dired-ediff-files))
     "TODO Two"
     (("F" dired-do-find-marked-files)
      ("G" dired-do-chgrp)
      ("g" revert-buffer)        ;; read all directories again (refresh)
      ("i" dired-maybe-insert-subdir)
      ("l" dired-do-redisplay)   ;; relist the marked or singel directory
      ("M" dired-do-chmod)
      ("m" dired-mark)
      ("O" dired-display-file)
      ("o" dired-find-file-other-window)
      ("Q" dired-do-find-regexp-and-replace)
      ("R" dired-do-rename))
     "TODO Three"
     (("r" dired-do-rsynch)
      ("S" dired-do-symlink)
      ("s" dired-sort-toggle-or-edit)
      ("t" dired-toggle-marks)
      ("U" dired-unmark-all-marks)
      ("u" dired-unmark)
      ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
      ("w" dired-kill-subdir)
      ("Y" dired-do-relsymlink)
      ("z" diredp-compress-this-file)
      ("Z" dired-do-compress))))
  :config
  (define-key dired-mode-map (kbd "C-c C-h") 'dired-hydra/body))

(provide 'init-dired)
