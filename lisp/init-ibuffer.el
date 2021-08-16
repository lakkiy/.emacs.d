;;; -*- lexical-binding: t -*-

(eat-package ibuffer-vc
  :straight t
  :init
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                vc-relative-file)))
  :hook
  (ibuffer-hook .
                (lambda ()
                  (ibuffer-vc-set-filter-groups-by-vc-root)
                  (unless (eq ibuffer-sorting-mode 'alphabetic)
                    (ibuffer-do-sort-by-alphabetic)))))

(provide 'init-ibuffer)
