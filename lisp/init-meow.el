;;; -*- lexical-binding: t -*-

(install-package 'meow)

(setq meow-esc-delay 0.001
      meow-keypad-leader-dispatch "C-c"
      meow-keypad-start-keys nil
      meow-replace-state-name-list
      '((normal . "N")
        (beacon . "B")
        (insert . "I")
        (motion . "M")
        (keypad . "K")))

(defun my/meow-setup ()
  (require 'meow)

  (meow-thing-register 'angle '(pair ("<") (">")) '(pair ("<") (">")))
  (add-to-list 'meow-char-thing-table '(?a . angle))
  (meow-thing-register 'backquote '(regexp "`" "`") '(regexp "`" "`"))
  (add-to-list 'meow-char-thing-table '(?` . backquote))

  ;; Optimize flymake for meow.
  (add-hook 'meow-insert-exit-hook (lambda () (setq flymake-no-changes-timeout 0.5)))
  (add-hook 'meow-insert-enter-hook (lambda () (setq flymake-no-changes-timeout nil)))

  (meow-setup-dvorak)
  (meow-setup-indicator)
  (meow-global-mode 1))

(add-hook 'after-init-hook #'my/meow-setup)

(defun meow-setup-dvorak ()
  (interactive)
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvorak)

  (meow-motion-overwrite-define-key
   '("<escape>" . mode-line-other-buffer)
   '("'" . repeat)
   '(")" . tab-bar-switch-to-prev-tab)
   '("}" . tab-bar-switch-to-next-tab))

  (meow-leader-define-key
   '("b" . switch-to-buffer)
   '("k" . kill-current-buffer)
   (cons "p" project-prefix-map)

   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument))

  (meow-normal-define-key
   '("S" . save-buffer)
   '("Q" . flymake-goto-prev-error)
   '("J" . flymake-goto-next-error)
   '(")" . tab-bar-switch-to-prev-tab)
   '("}" . tab-bar-switch-to-next-tab)
   '("-" . avy-goto-char-timer)
   '("\\" . golden-ratio)

   ;;; Below config basicly won't change anymore.
   '("'" . repeat)
   '("!" . treemacs)
   '(":" . gptel-menu)
   '("~" . eat-project)
   '("`" . eat)
   '("_" . xeft)
   '("@" . hs-toggle-hiding)
   '("q" . delete-window)
   '("/" . comment-dwim)
   '("v" . scroll-up-command)
   '("V" . scroll-down-command)
   '("\"" . insert-pair)
   '("[" . insert-pair)
   '("{" . insert-pair)
   '("(" . insert-pair)
   '("]" . delete-pair)

   '("?" . meow-cheatsheet)
   '("<escape>" . mode-line-other-buffer)
   '(";" . meow-reverse)

   ;; mark/find/till/visit, most used in beacon mode
   '("O" . meow-mark-word)
   '("E" . meow-mark-symbol)
   '("M" . meow-search)
   '("f" . meow-find)
   '("F" . meow-till)
   '("l" . meow-visit)

   ;; kill/delete/change/replace
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("k" . meow-kill)
   '("K" . meow-kill-whole-line)
   '("C" . meow-change)
   '("r" . meow-replace)

   ;; yank/pop
   '("s" . meow-save)
   '("y" . meow-yank)

   ;; grab
   '("G" . meow-grab)
   '("X" . meow-sync-grab)
   '("R" . meow-swap-grab)
   '("z" . meow-pop-selection)

   ;; undo
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)

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

   ;; line operation
   '("j" . meow-join)
   '("e" . meow-line)
   '("o" . meow-block)

   ;; query replace & kmacro
   '("#" . meow-start-kmacro-or-insert-counter)
   '("$" . meow-end-or-call-kmacro)
   '("&" . meow-query-replace)
   '("%" . meow-query-replace-regexp)

   ;; start keypad
   '("m" . meow-keypad-start)
   '("g" . meow-keypad-start)
   '("c" . meow-keypad-start)
   '("x" . meow-keypad-start)

   ;; thing
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)

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
   '("W" . meow-next-symbol)))

(provide 'init-meow)
