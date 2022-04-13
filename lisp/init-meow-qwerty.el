;; -*- lexical-binding: t -*-

;; NOTE this file won't update since switch to dvorak

(defun meow-setup-qwerty ()
  (interactive)
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
        +meow-layout 'qwerty)

  ;; TODO change to another function
  (meow-motion-overwrite-define-key
   '("<escape>" . +project-previous-buffer)
   '("'" . repeat)
   '(">" . tab-bar-switch-to-next-tab)
   '("<" . tab-bar-switch-to-prev-tab)
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
   '("e" . "C-x C-e")
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
   '("o" . delete-other-windows)
   '("O" . ace-delete-window)
   '("q" . delete-window)
   '("-" . split-window-below)
   '("\\" . split-window-right)

   ;; xref
   '("." . "M-.")
   '("z" . "M-,") ;; HACK `xref-pop-stack-mark' was replaced by `xref-go-back' after emacs28,
   '("," . "M-,")
   '("Z" . "M-?")
   '("?" . "M-?")

   ;; project
   (cons "p" project-prefix-map)

   ;; tab
   '("t t" . tab-bar-select-tab-by-name)
   '("t n" . tab-bar-new-tab)
   '("t r" . tab-bar-rename-tab)
   '("t k" . tab-bar-close-tab)

   ;; app
   '("d" . dired)
   '("v" . magit)
   '("r" . rg-project)

   ;; toggles
   '("$" . load-theme)
   '("L" . display-line-numbers-mode)
   '("A" . org-agenda-list)
   '("T" . telega)
   '("@" . treemacs-select-window)
   )

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
   '("v" . meow-line)
   '("V" . meow-goto-line)
   '("o" . meow-block)
   '("O" . meow-to-block)

   ;; yank/pop
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("p" . meow-yank)

   ;; grab
   '("G" . meow-grab)
   '("S" . meow-swap-grab)
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
