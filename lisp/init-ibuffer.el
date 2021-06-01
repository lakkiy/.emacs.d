;;; -*- lexical-binding: t -*-

(straight-use-package 'ibuffer-vc)
;; ibuffer-vc
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

(pretty-hydra-define hydra-ibuffer-mark (:title "Ibuffer Mark" :quit-key "q")
  ("Mark"
   (("*" ibuffer-unmark-all "unmark all")
    ("M" ibuffer-mark-by-mode "mode")
    ("m" ibuffer-mark-modified-buffers "modified")
    ("u" ibuffer-mark-unsaved-buffers "unsaved")
    ("s" ibuffer-mark-special-buffers "special")
    ("r" ibuffer-mark-read-only-buffers "read-only")
    ("/" ibuffer-mark-dired-buffers "dired")
    ("e" ibuffer-mark-dissociated-buffers "dissociated")
    ("h" ibuffer-mark-help-buffers "help")
    ("z" ibuffer-mark-compressed-file-buffers "compressed")
    ("b" hydra-ibuffer-main/body "back"))))

(pretty-hydra-define hydra-ibuffer-action (:title "Ibuffer Action" :quit-key "q")
  ("Action"
   (("A" ibuffer-do-view "view")
    ("E" ibuffer-do-eval "eval")
    ("F" ibuffer-do-shell-command-file "shell-command-file")
    ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
    ("H" ibuffer-do-view-other-frame "view-other-frame")
    ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
    ("M" ibuffer-do-toggle-modified "toggle-modified")
    ("O" ibuffer-do-occur "occur")
    ("P" ibuffer-do-print "print")
    ("Q" ibuffer-do-query-replace "query-replace")
    ("R" ibuffer-do-rename-uniquely "rename-uniquely")
    ("T" ibuffer-do-toggle-read-only "toggle-read-only")
    ("U" ibuffer-do-replace-regexp "replace-regexp")
    ("V" ibuffer-do-revert "revert")
    ("W" ibuffer-do-view-and-eval "view-and-eval")
    ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
    ("b" hydra-ibuffer-main/body "back"))))

(pretty-hydra-define hydra-ibuffer-sort (:title "Ibuffer Sort" :quit-key "q")
  ("Sort"
   (("i" ibuffer-invert-sorting "invert")
    ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
    ("v" ibuffer-do-sort-by-recency "recently used")
    ("s" ibuffer-do-sort-by-size "size")
    ("f" ibuffer-do-sort-by-filename/process "filename")
    ("m" ibuffer-do-sort-by-major-mode "mode")
    ("b" hydra-ibuffer-main/body "back"))))

(pretty-hydra-define hydra-ibuffer-filter (:tiele "Ibuffer Filter" :quit-key "q")
  ("Filter"
   (("m" ibuffer-filter-by-used-mode "mode")
    ("M" ibuffer-filter-by-derived-mode "derived mode")
    ("n" ibuffer-filter-by-name "name")
    ("c" ibuffer-filter-by-content "content")
    ("e" ibuffer-filter-by-predicate "predicate")
    ("f" ibuffer-filter-by-filename "filename")
    (">" ibuffer-filter-by-size-gt "size")
    ("<" ibuffer-filter-by-size-lt "size")
    ("/" ibuffer-filter-disable "disable")
    ("b" hydra-ibuffer-main/body "back"))))

(pretty-hydra-define ibuffer-hydra (:title "Ibuffer" :quit-key "q")
  ("Mark"
   (("m" ibuffer-mark-forward "mark")
    ("u" ibuffer-unmark-forward "unmark")
    ("*" hydra-ibuffer-mark/body "specific" :color blue)
    ("t" ibuffer-toggle-marks "toggle"))
   "Actions"
   (("D" ibuffer-do-delete "delete")
    ("s" ibuffer-do-save "save marked")
    ("a" hydra-ibuffer-action/body "all actions" :color blue))
   "View"
   (("g" ibuffer-update "refresh")
    ("S" hydra-ibuffer-sort/body "sort" :color blue)
    ("/" hydra-ibuffer-filter/body "filter" :color blue)
    ("H" describe-mode "help" :color blue))
   "Select"
   (("h" ibuffer-backward-filter-group "backward filter")
    ("l" ibuffer-forward-filter-group "forward filter")
    ("RET" ibuffer-visit-buffer "visit" :color blue)
    ("TAB" ibuffer-toggle-filter-group "toggle")
    ("o" ibuffer-visit-buffer-other-window "other window" :color blue))))

(with-eval-after-load "ibuffer"
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic))))
  (define-key ibuffer-mode-map (kbd "C-c C-h") 'ibuffer-hydra/body))

(provide 'init-ibuffer)
