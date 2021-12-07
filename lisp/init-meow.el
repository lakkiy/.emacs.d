;;; -*- lexical-binding: t -*-

(eat-package meow
  :straight t
  :hook
  (after-init-hook . (lambda ()
                       (meow-global-mode 1)))
  :config
  ;;; project.el use C-x p
  (global-unset-key (kbd "C-x C-p"))
  (global-set-key (kbd "C-x C-d") #'dired)
  ;; SPC x f to describe-funtion
  (global-set-key (kbd "C-h C-f") 'describe-funtion)
  (dolist (mode '(go-dot-mod-mode
                  diff-mode))
    (add-to-list 'meow-mode-state-list `(,mode . normal)))
  (global-set-key (kbd "C-s") 'meow-visit)
  (meow-setup)
  :init
  (setq meow-esc-delay 0.001)
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev))
    (meow-leader-define-key
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
     ;; TODO project search
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
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("w" . meow-next-word)
     '("W" . meow-next-symbol)
     '("x" . meow-delete)
     '("X" . meow-backward-delete)
     '("c" . meow-change)
     '("C" . meow-change-save)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("m" . meow-mark-word)
     '("M" . meow-mark-symbol)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("q" . meow-quit)
     '("r" . meow-replace)
     '("R" . meow-replace-save)
     '("n" . scroll-up-command)
     '("N" . scroll-down-command)
     ;; HACK default `undo' in undo in region
     ;;      `meow-undo' is undo in global
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-line)
     '("V" . meow-goto-line)
     '("d" . meow-kill)
     ;; may have some change
     '("f" . meow-visit) ;; HACK can not use in motion mode
     '("F" . meow-find)
     '("s" . meow-search)
     '("S" . meow-swap-grab)
     '("t" . meow-till)
     '("T" . meow-till-expand)
     '("e" . meow-join)
     '("E" . delete-indentation)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("p" . meow-yank)
     '("P" . meow-pop-grab)
     '("z" . meow-pop-selection)
     '("Z" . meow-pop-all-selection)
     '("?" . meow-cheatsheet)
     '("&" . meow-query-replace)
     '("%" . meow-query-replace-regexp)
     '("<f2>" . meow-quick-kmacro)
     '("<f3>" . meow-start-kmacro)
     '("<f4>" . meow-end-or-call-kmacro)
     '("<escape>" . mode-line-other-buffer)
     '("-" . hs-hide-block) ;; TODO `negative-argument'
     '("=" . hs-show-block)
     '("_" . hs-hide-all)
     '("+" . hs-show-all)
     '("'" . repeat)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("{" . flymake-goto-prev-error) ;; TODO wrap flymake and flycheck together
     '("}" . flymake-goto-next-error)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing))))

(provide 'init-meow)
