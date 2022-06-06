;;; -*- lexical-binding: t -*-
;; config should work under emacs -Q like straight and gc...

;;; Bootstrap straight.el
;; https://www.reddit.com/r/emacs/comments/mtb05k/emacs_init_time_decreased_65_after_i_realized_the/
(setq straight-check-for-modifications '(check-on-save find-when-checking))
(setq straight-vc-git-default-clone-depth 1)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(eat-package on
  :straight (on :type git :host github :repo "ajgrf/on.el")
  :init
  (require 'on))


;;; GC
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

(when eat/emacs29p
  ;; for mouse scroll
  (setq pixel-scroll-precision-large-scroll-height 60
        pixel-scroll-precision-interpolation-factor 30.0
        dired-mouse-drag-files t
        mouse-drag-and-drop-region t
        mouse-drag-and-drop-region-cross-program t)
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
(defun display-startup-echo-area-message()
  (message nil))

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

(setq
 initial-scratch-message (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n")
 initial-major-mode 'fundamental-mode               ; Don't use prog-mode an stratup
 ring-bell-function 'ignore
 read-process-output-max (* 4 1024 1024)
 suggest-key-bindings nil                           ; Disable "You can run the command balabala..."
 word-wrap-by-category t                            ; Emacs 之光！
 use-short-answers t                                ; yse-or-no -> y-or-n
 suggest-key-bindings nil
 )

(setq-default
 inhibit-compacting-font-caches t                   ; Don’t compact font caches during GC.
 require-final-newline t                            ; add final newline
 visible-cursor t
 bidi-inhibit-bpa t                                 ; Improve long line display performance
 bidi-paragraph-direction 'left-to-right
 echo-keystrokes 0.01                               ; don't wait for keystrokes display
 warning-suppress-log-types '((comp))               ; Don't display compile warnings
 truncate-partial-width-windows 65                  ; Don't truncate lines in a window narrower than 65 chars.
 vc-follow-symlinks t                               ; always follow link
 server-client-instructions nil                     ; no client startup messages
 split-height-threshold nil                         ; prefer horizental split
 split-width-threshold 120
 )


;;; Load custom-file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and (file-exists-p custom-file)
           (file-readable-p custom-file))
  (load custom-file :no-error :no-message))

(eat-package benchmark-init
  :straight t
  :init
  (when eat/enable-benchmark
    (benchmark-init/activate))
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(eat-package default-text-scale
  :straight t
  :init
  (global-set-key (kbd "C-x C-=") #'default-text-scale-increase)
  (global-set-key (kbd "C-x C--") #'default-text-scale-decrease))

;;; init-default.el ends here
(provide 'init-default)
