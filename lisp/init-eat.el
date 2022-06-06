;;; -*- lexical-binding: t -*-
;;; Basic
;;;; Variables

(defvar eat/user-full-name "Liu Bo")

(defvar eat/user-mail-address "liubolovelife@gmail.com")

(defvar eat/complete-delay 0.5
  "Delay time before complete.")

(defvar eat/enable-icon t
  "Whether to enable `all-the-icons'.")

(defvar eat/enable-benchmark nil
  "Enable `benchmark-init', run `benchmark-init/show-durations-tree' to see result.")

;;;; Consts
(defconst eat/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst eat/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst eat/emacs29p
  (>= emacs-major-version 29)
  "Emacs is 29 or above.")


;;; Frame
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

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

(defconst eat/initial-frame (selected-frame)
  "The frame (if any) active during Emacs initialization.")

(add-hook 'after-init-hook
          (lambda () (when eat/initial-frame
                       (run-after-make-frame-hooks eat/initial-frame))))


;;; Font
;;;; Variables
(defvar eat/fonts-default
  '("Roboto Mono" "Iosevka" "Menlo" "Source Code Pro")
  "First installed font will be set to default font.")

(defvar eat/fonts-unicode
  '("Apple Color Emoji" "Noto Color Emoji")
  "First installed font will be set to unicode font.")

(defvar eat/fonts-cn
  '("LXGW WenKai" "PingFang SC")
  "First installed font will be set to Chinese font.")

(defvar eat/fonts-variable-pitch
  '("Cardo" "Bookerly" "Nimbus Sans" "Helvetica")
  "First installed font will be set to variable font.")

(defvar eat/font-size 12
  "Default font size.")

;;;; Functions
(defun eat/font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun eat/font-installed (list)
  "Return first installed font from LIST."
  (catch 'value
    (dolist (font list)
      (when (eat/font-installed-p font)
        (throw 'value font)))))

(defun eat/show-startup-time ()
  "Print startup time."
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))
(add-hook 'emacs-startup-hook #'eat/show-startup-time)

;; GC automatically while unfocusing the frame
(add-function :after after-focus-change-function
              (lambda ()
                (unless (frame-focus-state)
                  (garbage-collect))))

(defun eat/load-base-font ()
  (let ((font-spec (format "%s-%d" eat/font-default eat/font-size)))
    (set-frame-font font-spec)
    (set-face-attribute 'default nil :font font-spec)
    (add-to-list 'default-frame-alist `(font . ,font-spec)))
  (set-fontset-font t '(#x4e00 . #x9fff) eat/font-cn))

(defun eat/load-face-font ()
  (set-face-attribute 'variable-pitch nil :font eat/font-variable-pitch :height 1.3)
  (set-face-attribute 'fixed-pitch nil :font eat/font-default)
  (set-face-attribute 'fixed-pitch-serif nil :font eat/font-default)
  ;; make mode line use variable font but use original height
  (custom-set-faces
   `(mode-line ((t (:family ,eat/font-variable-pitch))))
   `(mode-line-inactive ((t (:family ,eat/font-variable-pitch))))))

(defun eat/load-ext-font ()
  (let ((font (frame-parameter nil 'font))
        (font-spec (font-spec :family eat/font-unicode)))
    (dolist (charset '(kana han hangul cjk-misc bopomofo symbol))
      (set-fontset-font font charset font-spec)))
  (set-fontset-font t 'emoji (font-spec :family eat/font-unicode) nil 'prepend)
  (setf (alist-get eat/font-unicode face-font-rescale-alist 0.7 nil 'string=) 0.7))

(defun eat/load-font ()
  (eat/load-base-font)
  (eat/load-face-font)
  (eat/load-ext-font))

;; TUI: only load tui theme
(add-hook 'after-make-console-frame-hooks (lambda ()
                                            (when (fboundp 'menu-bar-mode)
                                              (menu-bar-mode -1))
                                            (eat/load-theme eat/theme-tui)))

;; GUI frame: load font and theme
(add-hook 'after-make-window-system-frame-hooks (lambda ()
                                                  (eat/load-font)
                                                  (eat/load-theme eat/theme)))

;;;; Consts

(defconst eat/font-default
  (eat/font-installed eat/fonts-default)
  "Default font.")

(defconst eat/font-unicode
  (eat/font-installed eat/fonts-unicode)
  "Unicode font.")

(defconst eat/font-cn
  (eat/font-installed eat/fonts-cn)
  "Chinese font.")

(defconst eat/font-variable-pitch
  (eat/font-installed eat/fonts-variable-pitch)
  "Variable pitch font.")


;;; Theme
;;;; Variables

(defvar eat/theme 'modus-operandi
  "Default theme.")

(defvar eat/theme-tui 'modus-vivendi
  "Default theme in terminal.")

(defvar eat/theme-system-light 'modus-operandi
  "Default light theme after system appearance changed.")

(defvar eat/theme-system-dark 'modus-vivendi
  "Default dark theme after system appearance changed.")

(defvar eat/theme-hooks nil
  "((theme-id . function) ...).")

;;;; Functions

(defun eat/load-theme (theme)
  "Load THEME without confirm."
  (load-theme theme t))

(defun eat/load-theme-advice (f theme-id &optional no-confirm no-enable &rest args)
  "Enhance `load-theme' by disabling other enabled themes & calling hooks."
  (unless no-enable ;
    (mapc #'disable-theme custom-enabled-themes))
  (prog1
      (apply f theme-id t no-enable args)
    (unless no-enable ;
      (pcase (assq theme-id eat/theme-hooks)
        (`(,_ . ,f) (funcall f))))))
(advice-add 'load-theme :around #'eat/load-theme-advice)

(defun eat/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha-background) 100))
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha-background newalpha))))))
(global-set-key (kbd "M-C-8") (lambda () (interactive) (eat/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (eat/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha-background . 100)))))


;;; eat-package
;;;; Commentary:

;; Base on luna-load-package.el

;;;; Code:

(require 'pcase)

;;;; Variables

(defvar eat-all-packages-daemon t
  "If it's value is t, all package in `eat-package' will be required in dameon.")

(defconst eat--all-packages-p
  (and eat-all-packages-daemon (daemonp))
  "")

;;;; Functions

(defun eat-package-split-command-args (args)
  "Split args into commands and args.
If ARGS is (:command args args args :command args),
return: ((:command . (args args args)) (:command . (args)))."
  (let (ret-list arg-list command)
    (dolist (token (append args '(:finish)))
      (if (keywordp token)
          ;; Finish previous command
          (progn (if command (push (cons command (reverse arg-list))
                                   ret-list))
                 (setq arg-list nil)
                 ;; Start new command
                 (setq command token))
        (push token arg-list)))
    (reverse ret-list)))

(defun eat-package--handle-hook (arg-list package)
  "Handle hook arguments.
Each ARG in ARG-LIST is a cons (HOOK . FUNCTION).
HOOK can be either a single hook or a list of hooks.
FUNCTION can also be either a single function or a list of them.
PACKAGE is the package we are configuring."
  (let (ret-list hook-list func-list)
    (dolist (arg arg-list)
      (let ((hook (car arg))
            (func (cdr arg)))
        ;; Normalize to lists.
        (setq hook-list
              (if (symbolp hook) (list hook) hook))
        (setq func-list
              (if (or (symbolp func)
                      ;; Handle lambda correctly.
                      (functionp func))
                  (list func) func)))
      ;; Produce add-hook forms.
      (dolist (func func-list)
        ;; If FUNC is a lambda function, we can't autoload it,
        ;; Make it load the package before execution.
        (let ((func (if (not (symbolp func))
                        ;; We don't want closure.
                        `(lambda () (require ',package) (funcall ,func))
                      func)))
          (dolist (hook hook-list)
            (push `(add-hook ',hook #',func) ret-list)))))
    (reverse ret-list)))

(defun eat-package--collect-autoload (arg-list package)
  "Collect functions that needs autoload from ARG-LIST.
PACKAGE is the package we are loading.
Return a list of (autoload ...) forms."
  (let ((autoload
          (mapcan (lambda (arg)
                    (let ((command (car arg))
                          (arg-list (cdr arg)))
                      (pcase command
                        ;; ARG is either (hook . fn) or
                        ;;               ((hook ...) . fn) or
                        ;;               (hook . (fn ...))
                        (:hook (mapcan (lambda (arg)
                                         (let ((fn (cdr arg)))
                                           (if (or (symbolp fn)
                                                   ;; Handle lambda.
                                                   (functionp fn))
                                               (list fn)
                                             fn)))
                                       arg-list))
                        ;; ARG is either ".xxx" or (".xxx" . mode)
                        (:mode (mapcar (lambda (arg)
                                         (if (stringp arg)
                                             package
                                           (cdr arg)))
                                       arg-list)))))
                  arg-list)))
    (mapcar (lambda (fn)
              (if (symbolp fn)
                  `(autoload #',fn ,(symbol-name package) nil t)))
            autoload)))

(defmacro eat-package (package &rest args)
  "Like ‘use-package’.
PACKAGE is the package you are loading.
Available COMMAND:

  :init         Run right away.
  :config       Run after package loads.
  :hook         Each arguments is (HOOK . FUNC)
                HOOK and FUNC can be a symbol or a list of symbols.
  :mode         Add (ARG . PACKAGE) to ‘auto-mode-alist’. If ARG is
                already a cons, add ARG to ‘auto-mode-alist’.
  :commands     Add autoload for this command.
  :after        Require after this package loads.
  :reqire       Require this package right now.
  :straight     Install package via straight

Each COMMAND can take zero or more ARG. Among these commands,
:hook, :commands, and :after expect literal arguments, :init,
:config expect s-expressions, which are evaluated after
expansion of the macro.

ARGS.

\(fn PACKAGE &rest [COMMAND [ARG ...]] ...)"
  (declare (indent 1))
  ;; Group commands and arguments together.
  (let* ((arg-list (eat-package-split-command-args args))
         ;; Translate commands & arguments to valid
         ;; config code.
         (body
          (mapcan
           (lambda (arg)
             (let ((command (car arg))
                   (arg-list (cdr arg)))
               (pcase command
                 (:straight `((if (listp ',@arg-list)
                                  (straight-use-package ',@arg-list)
                                (straight-use-package ',package))))
                 (:init arg-list)
                 (:config `((with-eval-after-load ',package
                              ,@arg-list)))
                 (:hook (eat-package--handle-hook arg-list package))
                 (:mode
                  ;; ARG is either ".xxx" or (".xxx" . mode)
                  (mapcar
                   (lambda (arg)
                     (let ((pattern (if (consp arg) (car arg) arg))
                           (mode-fn (if (consp arg) (cdr arg) package)))
                       `(add-to-list 'auto-mode-alist
                                     ',(cons pattern mode-fn))))
                   arg-list))
                 (:commands
                  (mapcar (lambda (cmd)
                            `(autoload ',cmd ,(symbol-name package) nil t))
                          arg-list))
                 (:after
                  (mapcar (lambda (pkg)
                            `(with-eval-after-load ',pkg
                               (require ',package)))
                          arg-list)))))
           arg-list))
         (autoload-list (eat-package--collect-autoload arg-list package))
         ;; Must :require explicitly if you want to require this package.
         (require-p (let ((commands (mapcar #'car arg-list)))
                      (or (memq :require commands)))))
    `(condition-case err
         (progn
           ,@autoload-list
           ,@body
           (if eat--all-packages-p
               (require ',package)
             ,(when require-p `(require ',package))))
       ((debug error) (warn "Error when loading %s: %s" ',package
                            (error-message-string err))))))


;;; Mac specific configuration
(when eat/macp
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        ;; Render thinner fonts
        ns-use-thin-smoothing t
        ;; Don't open a file in a new frame
        ns-pop-up-frames nil)
  (push '(ns-transparent-titlebar . t) default-frame-alist)

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

  ;; load theme after system appearance changed
  (when (boundp 'ns-system-appearance)
    (add-to-list 'ns-system-appearance-change-functions
                 (lambda (l?d)
                   (if (eq l?d 'light)
                       (eat/load-theme eat/theme-system-light)
                     (eat/load-theme eat/theme-system-dark)))))

  (global-set-key [(super a)] #'mark-whole-buffer)
  (global-set-key [(super v)] #'yank)
  (global-set-key [(super c)] #'kill-ring-save)
  (global-set-key [(super s)] #'save-buffer)
  (global-set-key [(super l)] #'goto-line)
  (global-set-key [(super w)] #'delete-frame)
  (global-set-key [(super q)] #'save-buffers-kill-terminal) ;; `save-buffers-kill-emacs' will shutdown emacs daemon
  (global-set-key [(super z)] #'undo))


;;; Linux specific configuration
(when eat/linuxp
  ;; NOTE use C-M-8 to set manually
  ;; (push '(alpha-background . 80) default-frame-alist)
  ;; Linux specific
  (setq x-underline-at-descent-line t)

  (setq-default
   pgtk-use-im-context-on-new-connection nil          ; Don't use Fcitx5 in Emacs in PGTK build
   x-gtk-resize-child-frames nil)

  ;; Don't use GTK+ tooltip
  (when (boundp 'x-gtk-use-system-tooltips)
    (setq x-gtk-use-system-tooltips nil)))


;;; Dvorak
;; Make “C-t” act like “C-x”, so it's easier to type on Dvorak layout
(keyboard-translate ?\C-t ?\C-x)
(keyboard-translate ?\C-x ?\C-t)


;;; Keybindings
;; bind `describe-keymap', added in emacs 28
(global-set-key (kbd "C-h C-k") #'describe-keymap)

;; this will stuck emacs
(global-unset-key (kbd "C-h h"))



;;; init-eat.el ends here
(provide 'init-eat)
