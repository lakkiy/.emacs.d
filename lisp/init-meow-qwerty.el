;; -*- lexical-binding: t -*-

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  ;; TODO change to another function
  (meow-motion-overwrite-define-key
   '("<escape>" . +project-previous-buffer)
   '("'" . repeat)
   ;; don't overwrite keys that already used in `meow-leader-define-key'
   ;; or `H-key' won't run the origin command in motion state
   '(">" . tab-bar-switch-to-next-tab)
   '("<" . tab-bar-switch-to-prev-tab))
  (meow-leader-define-key
   '(";" . "H-j") ;; run origin command in motion state
   '(":" . "H-k")
   '("e" . "C-x C-e")
   '("b" . switch-to-buffer)
   '(";" . comment-dwim)
   '("f" . find-file)
   '("d" . dired)
   '("s" . ispell-word)
   '("=" . align-regexp)
   '("$" . load-theme)
   '("k" . kill-this-buffer)
   '("i" . imenu)
   '("v" . magit)
   '("B" . ibuffer)
   '("r" . rg-project)
   ;; wrap && unwrap
   '("\"" . "M-\"")
   '("[" . "M-[")
   '("{" . "M-{")
   '("(" . "M-(")
   '(")" . "M-)") ;; unwrap
   ;; project
   '("p s" . project-search)
   '("p p" . project-switch-project)
   '("p f" . project-find-file)
   '("p b" . project-switch-to-buffer)
   '("p K" . project-kill-buffers)
   '("p e" . project-eshell)
   '("p d" . project-dired)
   ;; xref
   '("." . "M-.")
   '("z" . "M-,") ;; HACK `xref-pop-stack-mark' was replaced by `xref-go-back' after emacs28,
   '("," . "M-,")
   '("Z" . "M-?")
   '("?" . "M-?")
   ;; window
   '("w" . ace-window)
   '("W" . ace-swap-window)
   '("o" . delete-other-windows)
   '("O" . ace-delete-window)
   '("q" . delete-window)
   '("-" . split-window-below)
   '("\\" . split-window-right)
   ;; toggles
   '("L" . display-line-numbers-mode)
   '("A" . org-agenda-list)
   '("t" . tab-bar-select-tab-by-name)
   '("T" . telega)
   '("I" . imenu-list-smart-toggle)
   '("F" . auto-fill-mode)
   '("@" . treemacs-select-window))
  (meow-normal-define-key
   '("?" . meow-cheatsheet)
   '("<escape>" . +project-previous-buffer)
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
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
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
   '("s" . meow-search)
   ;; kill/delete/change/replace
   '("x" . meow-delete)
   '("X" . meow-backward-delete)
   '("d" . meow-kill)
   '("r" . meow-replace)
   '("R" . meow-replace-save)
   '("c" . meow-change)
   '("C" . meow-change-save)
   ;; line operation
   '("e" . meow-join)
   '("E" . delete-indentation)
   '("v" . meow-line)
   '("V" . meow-goto-line)
   '("o" . meow-block)
   '("O" . meow-to-block)
   ;; yank/pop
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("p" . meow-yank)
   ;; grab
   '("P" . meow-pop-grab)
   '("G" . meow-grab)
   '("S" . meow-swap-grab)
   '("z" . meow-pop-selection)
   '("Z" . meow-pop-all-selection)
   ;; query replace
   '("&" . meow-query-replace)
   '("%" . meow-query-replace-regexp)
   ;; thing
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   ;; find/search/till/visit
   '("f" . meow-visit)
   '("F" . meow-find)
   '("t" . meow-till)
   '("T" . meow-till-expand)
   ;; undo
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   ;;
   '("'" . repeat)
   '("n" . scroll-up-command)
   '("N" . scroll-down-command)
   '("-" . hs-hide-block) ;; TODO `negative-argument'
   '("=" . hs-show-block)
   '("_" . hs-hide-all)
   '("+" . hs-show-all)
   '("<" . tab-bar-switch-to-prev-tab)
   '(">" . tab-bar-switch-to-next-tab)
   '("{" . flymake-goto-prev-error) ;; TODO wrap flymake and flycheck together
   '("}" . flymake-goto-next-error)
   ))

(provide 'init-meow-qwerty)
