;;; -*- lexical-binding: t -*-

;; for customize
(setq-default
  ;; font
 *font* "Operator Mono SSm Lig"
 *font-cn* "WenQuanYi Micro Hei"
 *font-unicode* "Apple Color Emoji"
 *font-height* (cond ((eq system-type 'darwin) 150)
                     (t 130))
 footheme 'spacemacs-light)

(setq-default
 ;; Close up of MacOs
 ring-bell-function 'ignore
 ;; no start messages
 inhibit-startup-message t
 ;; don't read x resource file
 inhibit-x-resources t
 ;; no welcome screen
 inhibit-splash-screen t
 inhibit-startup-screen t
 ;; no startup messages
 inhibit-startup-echo-area-message t
 frame-inhibit-implied-resize t
 initial-scratch-message ""
 hl-line-sticky-flag nil
 ;; Don't create lockfiles
 create-lockfiles nil
 ;; UTF-8
 buffer-file-coding-system 'utf-8-unix
 default-file-name-coding-system 'utf-8-unix
 default-keyboard-coding-system 'utf-8-unix
 default-process-coding-system '(utf-8-unix . utf-8-unix)
 default-sendmail-coding-system 'utf-8-unix
 default-terminal-coding-system 'utf-8-unix
 ;; add final newline
 require-final-newline t
 ;; Disable auto save and backup
 make-backup-files nil
 auto-save-default nil
 auto-save-list-file-prefix nil
 ;; Xref no prompt
 xref-prompt-for-identifier nil
 ;; Mouse yank at point instead of click position.
 mouse-yank-at-point t
 ;; This fix the cursor movement lag
 auto-window-vscroll nil
 tab-width 4
 ;; Don't show cursor in non selected window.
 cursor-in-non-selected-windows nil
 comment-empty-lines t
 visible-cursor t
 ;; Window divider setup
 window-divider-default-right-width 1
 window-divider-default-bottom-width 0
 window-divider-default-places t
 ;; allow resize by pixels
 frame-resize-pixelwise t
 x-gtk-resize-child-frames nil
 x-underline-at-descent-line t
 ;; Improve long line display performance
 bidi-inhibit-bpa t
 bidi-paragraph-direction 'left-to-right
 ;; don't wait for keystrokes display
 echo-keystrokes 0.01
 show-paren-style 'parenthese
 ;; indent with whitespace by default
 indent-tabs-mode nil
 read-process-output-max (* 1024 1024)
 ;; Default line number width.
 display-line-numbers-width 3
 ;; Don't use Fcitx5 in Emacs in PGTK build
 pgtk-use-im-context-on-new-connection nil
 ;; Don't display compile warnings
 warning-suppress-log-types '((comp))
 ;; Don't truncate lines in a window narrower than 65 chars.
 truncate-partial-width-windows 65
 ;; always follow link
 vc-follow-symlinks t
 ;; tab bar
 tab-bar-show nil
 tab-bar-new-tab-choice "*scratch*"
 ;; Vertical Scroll
 scroll-step 1
 scroll-margin 15
 scroll-conservatively 101
 scroll-up-aggressively 0.01
 scroll-down-aggressively 0.01
 auto-window-vscroll nil
 fast-but-imprecise-scrolling nil
 mouse-wheel-scroll-amount '(1 ((shift) . 1))
 mouse-wheel-progressive-speed nil
 ;; Horizontal Scroll
 hscroll-step 1
 hscroll-margin 10
 ;; no client startup messages
 server-client-instructions nil
 ;; install hunspell and hunspell-en_US
 ispell-dictionary "en_US"
 ispell-program-name "hunspell"
 ispell-personal-dictionary (expand-file-name ".hunspell_dict.txt" user-emacs-directory)
 ;; custom file
 custom-file (expand-file-name "custom.el" user-emacs-directory)
 ;; electric-pair-mode
 electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit
 ;; show-paren-mode
 show-paren-when-point-in-periphery t
 show-paren-when-point-inside-paren t
 ;; yse-or-no -> y-or-n
 use-short-answers t
 ;; prefer horizental split
 split-height-threshold nil
 split-width-threshold 120)

;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; (add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'conf-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'subword-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-init-hook 'save-place-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'after-init-hook 'global-auto-revert-mode)
(add-hook 'after-init-hook 'global-so-long-mode)
(add-hook 'after-init-hook 'winner-mode)
(add-hook 'after-init-hook 'electric-pair-mode)
(add-hook 'after-init-hook 'show-paren-mode)

(defun +reopen-file-with-sudo ()
  (interactive)
  (find-alternate-file (format "/sudo::%s" (buffer-file-name))))

(global-set-key (kbd "C-x C-z") #'+reopen-file-with-sudo)
;; use mouse left click to find definitions
(global-unset-key (kbd "C-<down-mouse-1>"))
(global-set-key (kbd "C-<mouse-1>") #'xref-find-definitions-at-mouse)
;; ibuffer
(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") 'ibuffer)
;;; project.el use C-x p
(global-unset-key (kbd "C-x C-p"))
(global-set-key (kbd "C-x C-d") #'dired)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'which-key)

;; which-key
(setq
 which-key-idle-delay 1
 which-key-idle-secondary-delay 0.05)

(add-hook 'after-init-hook 'which-key-mode)

(with-eval-after-load "which-key"
  (global-set-key (kbd "<f5>") 'which-key-show-major-mode))

;; exec-path-from-shell
(when (memq window-system '(mac ns x))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; TODO narrow widen
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev))
  (meow-leader-define-key
   ;; completion
   '("SPC" . execute-extended-command)
   '("i" . consult-imenu)
   '("I" . imenu-list-smart-toggle)
   '("b" . consult-buffer)
   ;; basic
   '("v" . magit)
   '("t" . tab-bar-select-tab-by-name)
   '("d" . dired)
   '("L" . display-line-numbers-mode)
   '("S" . ispell-word)
   '("B" . ibuffer)
   ;; projectile
   '("p s" . consult-ripgrep)
   '("p p" . project-switch-project)
   '("p f" . project-find-file)
   '("p F" . project-find-regexp)
   '("p b" . project-switch-to-buffer)
   '("p K" . project-kill-buffers)
   '("p e" . project-eshell)
   '("p r" . project-query-replace-regexp)
   '("p d" . project-dired)
   ;; xref
   '("." . xref-find-definitions)
   '("z" . xref-pop-marker-stack)
   '("," . xref-pop-marker-stack)
   '("Z" . xref-find-references)
   '("?" . xref-find-references)
   ;; wrap
   '("'" . meow-wrap-string)
   '("(" . meow-wrap-round)
   '("[" . meow-wrap-square)
   '("{" . meow-wrap-curly)
   '("}" . meow-forward-barf)
   '(")" . meow-forward-slurp)
   '("e" . meow-eval-last-exp)
   '(";" . meow-comment)
   ;; window
   '("w" . ace-window)
   '("W" . ace-swap-window)
   '("o" . delete-other-windows)
   '("q" . delete-window)
   '("-" . split-window-below)
   '("\\" . split-window-right)
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . meow-motion-origin-command)
   '("k" . meow-motion-origin-command)
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
   '("-" . hs-hide-block)
   '("=" . hs-show-block)
   '("_" . hs-hide-all)
   '("+" . hs-show-all)
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
   '("'" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . meow-change-save)
   '("d" . meow-kill)
   '("x" . meow-delete)
   '("f" . meow-find)
   '("F" . meow-find-expand)
   '("g" . meow-keyboard-quit)
   '("G" . goto-line)
   '("h" . meow-head)
   '("H" . meow-head-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("s" . meow-search)
   '("S" . meow-pop-search)
   '("t" . meow-till)
   '("T" . meow-till-expand)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-block-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("R" . meow-replace-save)
   '("l" . meow-tail)
   '("L" . meow-tail-expand)
   '("n" . scroll-up-command)
   '("N" . scroll-down-command)
   '("u" . undo)
   '("U" . undo-redo)
   '("v" . meow-line)
   '("V" . meow-visit)
   '("e" . meow-join)
   '("E" . delete-indentation)
   '("y" . meow-save)
   '("p" . meow-yank)
   '("z" . meow-pop-selection)
   '("Z" . meow-pop-all-selection)
   '("?" . meow-cheatsheet)
   '("&" . meow-query-replace)
   '("%" . meow-query-replace-regexp)
   '("<f2>" . meow-quick-kmacro)
   '("<f3>" . meow-start-kmacro)
   '("<f4>" . meow-end-or-call-kmacro)
   '("<escape>" . meow-last-buffer)))

(straight-use-package '(meow :type git :host github :repo "DogLooksGood/meow"))

;; meow
(require 'meow)

(meow-global-mode 1)

(with-eval-after-load "meow"
  ;; 狗哥说这个 gui 下也能 C-[ 退回到 normal
  (add-hook 'meow-mode-hook #'meow-esc-mode)
  (meow-setup-line-number)
  (meow-setup))

(provide 'init-core)
