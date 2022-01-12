;;; -*- lexical-binding: t -*-

;; Notifications
;;
;; Actually, `notify-send' is not defined in notifications package, but the
;; autoload cookie will make Emacs load `notifications' first, then our
;; `defalias' will be evaluated.
(pcase system-type
  ('gnu/linux
   (autoload #'notify-send "notifications")
   (with-eval-after-load "notifications"
     (defalias 'notify-send 'notifications-notify)))
  ('darwin
   ;; HACK you must enable notify for emacs in macos system
   ;;      Notifications & Focus -> Emacs -> Allow Notifications
   (defun notify-send (&rest params)
     "Send notifications via `terminal-notifier'."
     (let ((title (plist-get params :title))
           (body (plist-get params :body)))
       (start-process "terminal-notifier"
                      nil
                      "terminal-notifier"
                      "-group" "Emacs"
                      "-title" title
                      "-message" body
                      ;; FIXME this option didn't show emacs icon
                      ;; but -sender didn't show the message when focus on emacs
                      "-activate" "org.gnu.Emacs"))))
  (_
   (defalias 'notify-send 'ignore)))

;; Speed up startup
(setq auto-mode-case-fold nil)

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

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

  ;; FIXME
  (unless (eq system-type 'darwin)
    (setq command-line-ns-option-alist nil)
    ;; TODO use `cond' to set this for different system
    (setq interprogram-cut-function 'paste-to-osx)
    (setq interprogram-paste-function 'copy-from-osx))
  (unless (eq system-type 'gnu/linux)
    (setq command-line-x-option-alist nil)))

(defun +reopen-file-with-sudo ()
  (interactive)
  (find-alternate-file (format "/sudo::%s" (buffer-file-name))))
(global-set-key (kbd "C-x C-z") #'+reopen-file-with-sudo)

;; in emacs29 from Po Lu!
(when (boundp pixel-scroll-precision-mode)
  ;; for mouse scroll
  (setq pixel-scroll-precision-large-scroll-height 60)
  (setq pixel-scroll-precision-interpolation-factor 30.0)
  (pixel-scroll-precision-mode))

;; produces a cleaner result
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)

;; This `view-hello-file' always stack me
(global-unset-key (kbd "C-h h"))

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
  (when (eq system-type 'darwin)
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
 scroll-margin 10
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

(defun +load-theme-advice (f theme-id &optional no-confirm no-enable &rest args)
  "Enhance `load-theme' by disabling other enabled themes & calling hooks"
  (unless no-enable ;
    (mapc #'disable-theme custom-enabled-themes))
  (prog1
      (apply f theme-id no-confirm no-enable args)
    (unless no-enable ;
      (pcase (assq theme-id +theme-hooks)
        (`(,_ . ,f) (funcall f))))))
(advice-add 'load-theme :around #'+load-theme-advice)

(when (and (boundp 'ns-system-appearance) (display-graphic-p) +theme-system-appearance)
  (add-to-list 'ns-system-appearance-change-functions
               (lambda (l?d)
                 (if (eq l?d 'light)
                     (load-theme +theme-system-light t)
                   (load-theme +theme-system-dark t)))))

(defun w/see-you ()
  "Highlight ZERO WIDTH chars in all buffers."
  (interactive)
  (let ((charnames (list "BYTE ORDER MARK"
                         "ZERO WIDTH NO-BREAK SPACE"
                         "ZERO WIDTH SPACE"
                         "RIGHT-TO-LEFT MARK"
                         "RIGHT-TO-LEFT OVERRIDE"
                         "LEFT-TO-RIGHT MARK"
                         "OBJECT REPLACEMENT CHARACTER"

                         "ZERO WIDTH JOINER"
                         "ZERO WIDTH NON-JOINER")))
    (set-face-background 'glyphless-char "RoyalBlue1")
    (dolist (name charnames)
      ;; see info node "info:elisp#Glyphless Chars" for available values
      (set-char-table-range glyphless-char-display
                            (char-from-name name) "fuck"))))

(provide 'init-basic)
