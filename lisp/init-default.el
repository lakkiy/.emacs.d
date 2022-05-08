;;; -*- lexical-binding: t -*-
;; config should work under emacs -Q like straight and gc...

;;; Benchmark

(add-hook 'emacs-startup-hook
          (lambda ()
            (message
             "Emacs loaded in %s with %d garbage collections."
             (format
              "%.2f seconds"
              (float-time
               (time-subtract after-init-time before-init-time)))
             gcs-done)))

(eat-package on
  :straight (on :type git :host github :repo "ajgrf/on.el")
  :init
  (require 'on))

;;; Frame
(defvar after-make-console-frame-hooks '()
  "Hooks to run after creating a new TTY frame")
(defvar after-make-window-system-frame-hooks '()
  "Hooks to run after creating a new window-system frame")

(defun run-after-make-frame-hooks (frame)
  "Run configured hooks in response to the newly-created FRAME.
Selectively runs either `after-make-console-frame-hooks' or
`after-make-window-system-frame-hooks'"
  (with-selected-frame frame
    (run-hooks (if window-system
                   'after-make-window-system-frame-hooks
                 'after-make-console-frame-hooks))))

(add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)

(defconst sanityinc/initial-frame (selected-frame)
  "The frame (if any) active during Emacs initialization.")

(add-hook 'after-init-hook
          (lambda () (when sanityinc/initial-frame
                       (run-after-make-frame-hooks sanityinc/initial-frame))))

;;; Consts
(defconst *is-a-mac*
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst emacs/>=29p
  (>= emacs-major-version 29)
  "Emacs is 29 or above.")

(defun +load-theme ()
  (load-theme +theme t))

;;; Mac specific configuration
(when *is-a-mac*
  (when (boundp 'ns-system-appearance)
    (defun +load-theme ()
      (add-to-list 'ns-system-appearance-change-functions
                   (lambda (l?d)
                     (if (eq l?d 'light)
                         (load-theme +theme-system-light t)
                       (load-theme +theme-system-dark t))))))
  ;; https://emacs-china.org/t/emacs-mac-port-profile/2895/29?u=rua
  ;; NOTE: When PATH is changed, run the following command
  ;; $ sh -c 'printf "%s" "$PATH"' > ~/.path
  (condition-case err
      (let ((path (with-temp-buffer
                    (insert-file-contents-literally "~/.path")
                    (buffer-string))))
        (setenv "PATH" path)
        (setq exec-path (append (parse-colon-path path) (list exec-directory))))
    (error (warn "%s" (error-message-string err))))

  (push '(ns-transparent-titlebar . t) default-frame-alist)

  (setq mac-option-modifier 'meta
        mac-command-modifier 'super)

  (global-set-key [(super a)] #'mark-whole-buffer)
  (global-set-key [(super v)] #'yank)
  (global-set-key [(super c)] #'kill-ring-save)
  (global-set-key [(super s)] #'save-buffer)
  (global-set-key [(super l)] #'goto-line)
  (global-set-key [(super w)] #'delete-frame)
  (global-set-key [(super q)] #'save-buffers-kill-terminal) ;; `save-buffers-kill-emacs' will shutdown emacs daemon
  (global-set-key [(super z)] #'undo)

  ;;; Opacity
  (defun sanityinc/adjust-opacity (frame incr)
    "Adjust the background opacity of FRAME by increment INCR."
    (unless (display-graphic-p frame)
      (error "Cannot adjust opacity of this frame"))
    (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
           ;; The 'alpha frame param became a pair at some point in
           ;; emacs 24.x, e.g. (100 100)
           (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
           (newalpha (+ incr oldalpha)))
      (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
        (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

  (when (fboundp 'toggle-frame-fullscreen)
    ;; Command-Option-f to toggle fullscreen mode
    ;; Hint: Customize `ns-use-native-fullscreen'
    (global-set-key (kbd "M-ƒ") 'toggle-frame-fullscreen))

  (global-set-key (kbd "M-C-8") (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
  (global-set-key (kbd "M-C-9") (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
  (global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))
  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
  ;; FIXME 乱码了
  ;; (setq interprogram-cut-function 'paste-to-osx)
  ;; (setq interprogram-paste-function 'copy-from-osx)

  ;; Render thinner fonts
  (setq ns-use-thin-smoothing t)
  ;; Don't open a file in a new frame
  (setq ns-pop-up-frames nil))

(unless *is-a-mac*
  (setq command-line-ns-option-alist nil))

;;; Linux specific configuration
(when sys/linuxp
  ;; TODO not works well for light theme
  (push '(alpha-background . 80) default-frame-alist)
  ;; Linux specific
  (setq x-underline-at-descent-line t)

  (setq-default
   pgtk-use-im-context-on-new-connection nil          ; Don't use Fcitx5 in Emacs in PGTK build
   x-gtk-resize-child-frames nil)

  ;; Don't use GTK+ tooltip
  (when (boundp 'x-gtk-use-system-tooltips)
    (setq x-gtk-use-system-tooltips nil)))

(unless sys/linuxp
  (setq command-line-x-option-alist nil))

;;; GC

;; GC automatically while unfocusing the frame
(add-function :after after-focus-change-function
              (lambda ()
                (unless (frame-focus-state)
                  (garbage-collect))))

(eat-package gcmh
  :straight t
  :hook (after-init-hook . gcmh-mode)
  :init
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold #x6400000)) ;; 100 MB

;;; UTF-8

;; Contrary to what many Emacs users have in their configs, you don't need
;; more than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")

;;; Speed up startup

(setq auto-mode-case-fold nil)

;; Optimization
(setq idle-update-delay 1.0)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;;; Scroll

;; The nano style for truncated long lines.
(setq auto-hscroll-mode 'current-line)

(when emacs/>=29p
  ;; for mouse scroll
  (setq pixel-scroll-precision-large-scroll-height 60)
  (setq pixel-scroll-precision-interpolation-factor 30.0)
  (add-hook 'on-init-ui-hook (lambda () (pixel-scroll-precision-mode))))

;; scroll nand hscroll
(setq-default
 scroll-step 2
 scroll-margin 2
 hscroll-step 2                                     ; Horizontal Scroll
 hscroll-margin 2
 scroll-conservatively 101
 scroll-up-aggressively 0.01
 scroll-down-aggressively 0.01
 scroll-preserve-screen-position 'always
 auto-window-vscroll nil
 fast-but-imprecise-scrolling nil
 mouse-wheel-scroll-amount '(1 ((shift) . hscroll)) ; use shift + mouse wheel to scrll horizontally
 mouse-wheel-progressive-speed nil)

;;; Cursor

;; Disable cursor blink
(add-hook 'on-init-ui-hook (lambda () (blink-cursor-mode -1)))


;; Do not show cursor in nonselected windows
(setq-default cursor-in-non-selected-windows nil)

;;; GUI features

;; Suppress GUI features and more
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-splash-screen t
      inhibit-x-resources t
      inhibit-default-init t
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-buffer-menu t)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;; Pixelwise resize
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

;;; Start up message/screen

;; Shut up!
(defun display-startup-echo-area-message() (message nil))


;;; Indent tab

;; indent with whitespace by default
(setq-default
 tab-width 4
 indent-tabs-mode nil)


;;; Disable default auto backup and save file
(setq-default
 create-lockfiles nil                               ; Don't create lockfiles
 make-backup-files nil                              ; Disable auto save and backup
 auto-save-default nil
 auto-save-list-file-prefix nil)


;;; Misc

;; Do not highlight symbol in nonselected windows, see `highlight-symbol-at-point'
;; (setq highlight-nonselected-windows nil)

;; TODO set line height, but `line-spacing' only add space below line

(setq-default
 initial-scratch-message (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n")
 initial-major-mode 'fundamental-mode
 inhibit-compacting-font-caches t                   ; Don’t compact font caches during GC.
 ring-bell-function 'ignore                         ; Disable osx bell ring
 require-final-newline t                            ; add final newline
 mouse-yank-at-point t                              ; Mouse yank at point instead of click position.
 comment-empty-lines t
 visible-cursor t
 bidi-inhibit-bpa t                                 ; Improve long line display performance
 bidi-paragraph-direction 'left-to-right
 echo-keystrokes 0.01                               ; don't wait for keystrokes display
 read-process-output-max (* 4 1024 1024)
 warning-suppress-log-types '((comp))               ; Don't display compile warnings
 truncate-partial-width-windows 65                  ; Don't truncate lines in a window narrower than 65 chars.
 vc-follow-symlinks t                               ; always follow link
 server-client-instructions nil                     ; no client startup messages
 use-short-answers t                                ; yse-or-no -> y-or-n
 split-height-threshold nil                         ; prefer horizental split
 split-width-threshold 120
 suggest-key-bindings nil                           ; disable "You can run the command balabala..."
 word-wrap-by-category t ;; Emacs 之光！
 )

;;; bind `describe-keymap', added in emacs 28
(global-set-key (kbd "C-h C-k") #'describe-keymap)

;;; Load custom-file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and (file-exists-p custom-file)
           (file-readable-p custom-file))
  (load custom-file :no-error :no-message))

;;; Dvorak keyboard layout

;; Make “C-t” act like “C-x”, so it's easier to type on Dvorak layout
(keyboard-translate ?\C-t ?\C-x)
(keyboard-translate ?\C-x ?\C-t)

;;; for emacs 29
(setq
 ;; 'e' on a value in *Help* will pop you to a new buffer where you can edit the value.
 help-enable-variable-value-editing t)

(eat-package benchmark-init
  :straight t
  :init
  (when +enable-benchmark
    (benchmark-init/activate))
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(defconst +font-default (my/font-installed +fonts-default))
(defconst +font-unicode (my/font-installed +fonts-unicode))
(defconst +font-cn (my/font-installed +fonts-cn))
(defconst +font-variable-pitch (my/font-installed +fonts-variable-pitch))

;;; init-default.el ends here
(provide 'init-default)
