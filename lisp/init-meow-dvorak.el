;; -*- lexical-binding: t -*-

(defun meow-setup-dvorak ()
  (interactive)
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvorak
        +meow-layout 'dvorak)

  (meow-motion-overwrite-define-key
   '("<escape>" . mode-line-other-buffer)
   '("'" . repeat)
   '(")" . tab-bar-switch-to-prev-tab)
   '("}" . tab-bar-switch-to-next-tab)
   )

  ;; NOTE key defined in leader same as bind to C-c
  ;; so make sure it didn't conflict with keybindings
  ;; defined in other files or bulitin that start with C-c
  ;; or use this to make it start with C-c m
  (defalias 'meow-leader-command-prefix (make-sparse-keymap))
  (defvar meow-leader-map (symbol-function 'meow-leader-command-prefix)
    "Keymap for characters following C-c m.")
  (define-key global-map "\C-c\ m" 'meow-leader-command-prefix)
  (add-to-list 'meow-keymap-alist (cons 'leader 'meow-leader-command-prefix))

  (meow-leader-define-key
   '("a" . execute-extended-command)
   '("e" . eval-last-sexp)
   '(";" . comment-dwim)

   ;; file
   '("f" . find-file)
   '("F" . find-file-other-window)

   ;; buffer
   '("b" . switch-to-buffer)
   '("B" . switch-to-buffer-other-window)
   '("k" . kill-this-buffer)

   ;; window
   '("w" . ace-window)
   '("W" . ace-swap-window)
   '("o" . "C-x 1")
   '("O" . ace-delete-window)
   '("q" . delete-window)
   '("-" . "C-x 2")
   '("s" . "C-x 3")

   ;; xref
   '("." . "M-.")
   '("," . "M-,")
   '("?" . "M-?")

   ;; project
   '("p s" . project-find-regexp)
   '("p p" . project-switch-project)
   '("p f" . project-find-file)
   '("p b" . project-switch-to-buffer)
   '("p K" . project-kill-buffers)
   '("p e" . project-eshell)
   '("p d" . project-dired)

   ;; tab
   '("t t" . tab-bar-select-tab-by-name)
   '("t n" . tab-bar-new-tab)
   '("t r" . tab-bar-rename-tab)
   '("t k" . tab-bar-close-tab)

   ;; app
   '("d" . dired)
   '("v" . magit)
   '("r" . rg-project)
   '("C" . xeft)

   ;; toggles
   '("$" . load-theme)
   '("L" . display-line-numbers-mode)
   '("@" . treemacs-select-window)
   '("A" . org-agenda-list)
   '("T" . telega)
   )

  (meow-normal-define-key
   '("?" . meow-cheatsheet)
   '("<escape>" . mode-line-other-buffer)
   '(";" . meow-reverse)
   '("g" . meow-cancel-selection)
   '("q" . meow-quit)

   ;; expand by numbers
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)

   ;; movement, like hjkl
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("t" . meow-right)
   '("T" . meow-right-expand)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)

   ;; insert above/below
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("A" . meow-open-below)
   '("a" . meow-append)

   ;; move/mark by word/symbol
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)

   ;; kill/delete/change/replace
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("k" . meow-kill)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("c" . meow-change)

   ;; line operation
   '("j" . meow-join)
   '("e" . meow-line) ;; NOTE F3 or insert/append/change in grab to enable "every n line" grab
   '("E" . meow-goto-line)
   '("o" . meow-block)
   '("O" . meow-to-block)

   ;; yank/pop
   '("x" . meow-save)
   '("X" . meow-sync-grab)
   '("y" . meow-yank)

   ;; grab
   '("G" . meow-grab)
   '("z" . meow-pop-selection)

   ;; query replace
   '("&" . meow-query-replace)
   '("%" . meow-query-replace-regexp)

   ;; thing
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)

   ;; find/till/visit, most used in beacon mode
   '("/" . meow-search)
   '("F" . meow-find)
   '("L" . meow-till)
   '("l" . meow-visit)

   ;; undo
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)


   ;;
   '(":" . execute-extended-command)

   ;; scroll
   '("v" . scroll-up-command)
   '("V" . scroll-down-command)

   ;; buffer
   '("S" . save-buffer)

   ;; window
   '("s" . ace-window)

   ;; wrap && unwrap
   '("\"" . insert-pair)
   '("[" . insert-pair)
   '("{" . insert-pair)
   '("(" . insert-pair)
   '("]" . delete-pair) ;; NOTE maybe custom `delete-pair-blink-delay'

   ;; hide-show
   '("-" . hs-hide-block)
   '("=" . hs-show-block)
   '("_" . hs-hide-all)
   '("+" . hs-show-all)

   ;; flymake
   '("Q" . flymake-goto-prev-error)
   '("J" . flymake-goto-next-error)

   ;; tab-bar
   '(")" . tab-bar-switch-to-prev-tab)
   '("}" . tab-bar-switch-to-next-tab)

   ;; misc
   '("'" . repeat)
   ))

(provide 'init-meow-dvorak)
