;;; -*- lexical-binding: t -*-

(with-no-warnings
  ;; Don't ping things that look like domain names.
  (setq ffap-machine-p-known 'reject)

  (when (eq window-system 'mac)
    ;; Compatible with Emacs Mac port
    (setq mac-option-modifier 'meta
          mac-command-modifier 'super)
    (global-set-key [(super a)] #'mark-whole-buffer)
    (global-set-key [(super v)] #'yank)
    (global-set-key [(super c)] #'kill-ring-save)
    (global-set-key [(super s)] #'save-buffer)
    (global-set-key [(super l)] #'goto-line)
    (global-set-key [(super w)] #'delete-frame)
    (global-set-key [(super q)] #'save-buffers-kill-emacs)
    (global-set-key [(super z)] #'undo))

  (unless sys/macp
    (setq command-line-ns-option-alist nil))
  (unless sys/linuxp
    (setq command-line-x-option-alist nil)))

(defun +reopen-file-with-sudo ()
  (interactive)
  (find-alternate-file (format "/sudo::%s" (buffer-file-name))))
(global-set-key (kbd "C-x C-z") #'+reopen-file-with-sudo)

;; in emacs29 from Po Lu!
(when (boundp pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode))

;; produces a cleaner result
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)


;; Encoding
;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)


(add-hook 'after-init-hook (lambda () (blink-cursor-mode -1)))

;; Optimization
(setq idle-update-delay 1.0)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;; Suppress GUI features and more
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-splash-screen t
      inhibit-x-resources t
      inhibit-default-init t
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-buffer-menu t)

;; Pixelwise resize
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

(with-no-warnings
  (when sys/macp
    ;; Render thinner fonts
    (setq ns-use-thin-smoothing t)
    ;; Don't open a file in a new frame
    (setq ns-pop-up-frames nil)))

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

;; Linux specific
(setq x-underline-at-descent-line t)


;; Nice window divider
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?┃))


(setq-default
 comment-auto-fill-only-comments t
 initial-major-mode 'fundamental-mode
 inhibit-compacting-font-caches t                   ; Don’t compact font caches during GC.
 delete-by-moving-to-trash t                        ; Deleting files go to OS's trash folder
 ring-bell-function 'ignore                         ; Disable osx bell ring
 hl-line-sticky-flag nil
 create-lockfiles nil                               ; Don't create lockfiles
 require-final-newline t                            ; add final newline
 make-backup-files nil                              ; Disable auto save and backup
 auto-save-default nil
 auto-save-list-file-prefix nil
 mouse-yank-at-point t                              ; Mouse yank at point instead of click position.
 auto-window-vscroll nil                            ; This fix the cursor movement lag
 tab-width 4
 comment-empty-lines t
 visible-cursor t
 window-divider-default-right-width 1               ; Window divider setup
 window-divider-default-bottom-width 0
 window-divider-default-places t
 x-gtk-resize-child-frames nil
 x-underline-at-descent-line t
 bidi-inhibit-bpa t                                 ; Improve long line display performance
 bidi-paragraph-direction 'left-to-right
 echo-keystrokes 0.01                               ; don't wait for keystrokes display
 indent-tabs-mode nil                               ; indent with whitespace by default
 read-process-output-max (* 1024 1024)
 display-line-numbers-width 3                       ; Default line number width.
 pgtk-use-im-context-on-new-connection nil          ; Don't use Fcitx5 in Emacs in PGTK build
 warning-suppress-log-types '((comp))               ; Don't display compile warnings
 truncate-partial-width-windows 65                  ; Don't truncate lines in a window narrower than 65 chars.
 vc-follow-symlinks t                               ; always follow link
 scroll-step 1                                      ; Vertical Scroll
 scroll-margin 0
 scroll-conservatively 100000
 scroll-up-aggressively 0.01
 scroll-down-aggressively 0.01
 scroll-preserve-screen-position t
 auto-window-vscroll nil
 fast-but-imprecise-scrolling nil
 mouse-wheel-scroll-amount '(1 ((shift) . hscroll)) ; use shift + mouse wheel to scrll horizontally
 mouse-wheel-progressive-speed nil
 hscroll-step 1                                     ; Horizontal Scroll
 hscroll-margin 10
 server-client-instructions nil                     ; no client startup messages
 use-short-answers t                                ; yse-or-no -> y-or-n
 split-height-threshold nil                         ; prefer horizental split
 split-width-threshold 120
 suggest-key-bindings nil                           ; disable "You can run the command balabala..."
 word-wrap-by-category t ;; Emacs 之光！
 )

(provide 'init-basic)
