;; -*- lexical-binding: t -*-

(defun meow-setup-dvorak ()
  (interactive)
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvorak
        +meow-layout 'dvorak)
  ;; TODO change to another function
  (meow-motion-overwrite-define-key
   '("<escape>" . +project-previous-buffer)
   '("'" . repeat)
   ;; don't overwrite keys that already used in `meow-leader-define-key'
   ;; or `H-key' won't run the origin command in motion state
   '("{" . tab-bar-switch-to-prev-tab)
   '("}" . tab-bar-switch-to-next-tab))
  (meow-leader-define-key
   '("a" . execute-extended-command)
   '("e" . "C-x C-e")
   '("b" . switch-to-buffer)
   '("g" . keyboard-escape-quit) ;; NOTE close the fucking minibuffer
   '(";" . comment-dwim)
   '("f" . find-file)
   '("d" . dired)
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
   ;; project TODO
   '("p s" . project-search)
   '("p p" . project-switch-project)
   '("p f" . project-find-file)
   '("p b" . project-switch-to-buffer)
   '("p K" . project-kill-buffers)
   '("p e" . project-eshell)
   '("p d" . project-dired)
   ;; xref
   '("." . "M-.")
   '("," . "M-,")
   '("?" . "M-?")
   ;; window
   '("w" . ace-window)
   '("W" . ace-swap-window)
   '("o" . delete-other-windows)
   '("O" . ace-delete-window)
   '("q" . delete-window)
   '("-" . split-window-below)
   '("s" . split-window-right)
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
   '("/" . meow-search)
   ;; kill/delete/change/replace
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("k" . meow-kill)
   '("r" . meow-replace)
   '("R" . meow-swap-grab) ;; TODO move to grab
   '("c" . meow-change)
   ;; line operation
   '("j" . meow-join)
   '("e" . meow-line)
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
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   ;; find/search/till/visit
   '("v" . meow-visit)
   '("f" . meow-find)
   '("l" . meow-till)
   ;; undo
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   ;;
   '("'" . repeat)
   ;; '("n" . scroll-up-command) ;; TODO conflit
   ;; '("N" . scroll-down-command)
   '("-" . hs-hide-block) ;; TODO `negative-argument'
   '("=" . hs-show-block)
   '("_" . hs-hide-all)
   '("+" . hs-show-all)
   '("<" . flymake-goto-prev-error)
   '(">" . flymake-goto-next-error)
   '("{" . tab-bar-switch-to-prev-tab) ;; TODO wrap flymake and flycheck together
   '("}" . tab-bar-switch-to-next-tab)
   '("S" . save-buffer)
   '("s" . ace-window)
   ))

(provide 'init-meow-dvorak)
