;;; -*- lexical-binding: t -*-

;;; Basic
;;;; Variables

(defvar eat/user-full-name "Liu Bo")

(defvar eat/user-mail-address "liubolovelife@gmail.com")

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

(defun eat/delete-to-the-begining ()
  (interactive)
  (delete-region (point-min) (point)))

(defun eat/delete-to-the-end ()
  (interactive)
  (delete-region (point) (point-max)))

(defun eat/delete-whole-buffer ()
  (interactive)
  (delete-region (point-min) (point-max)))

;;;; Network Proxy
(defvar eat/proxy "127.0.0.1:7890"
  "Network proxy address.")

(defun eat/proxy-show ()
  "Show proxy."
  (interactive)
  (if (or url-proxy-services (bound-and-true-p socks-noproxy))
      (message "Current proxy is `%s'" eat/proxy)
    (message "No proxy")))

(defun eat/proxy-enable ()
  "Enable proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,eat/proxy)
          ("https" . ,eat/proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (require 'socks)
  (setq url-gateway-method 'socks
        socks-noproxy '("localhost"))
  (let* ((proxy (split-string eat/proxy ":"))
         (host (car proxy))
         (port (string-to-number (cadr proxy))))
    (setq socks-server `("Default server" ,host ,port 5)))
  (setenv "all_proxy" (concat "socks5://" eat/proxy))
  (eat/proxy-show))

(defun eat/proxy-disable ()
  "Disable proxy."
  (interactive)
  (setq url-proxy-services nil)
  (setq url-gateway-method 'native
        socks-noproxy nil
        socks-server nil)
  (setenv "all_proxy" "")
  (eat/proxy-show))

(defun eat/proxy-toggle ()
  "Toggle proxy."
  (interactive)
  (if (or (bound-and-true-p url-proxy-services) (bound-and-true-p socks-noproxy))
      (eat/proxy-disable)
    (eat/proxy-enable)))

;;;; Some useful functions
(defun eat/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun eat/rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun get-string-from-file (filePath)
  "Return file content as string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))


;;; Load custom-file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and (file-exists-p custom-file)
           (file-readable-p custom-file))
  (load custom-file :no-error :no-message))


;;; Frame
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(defvar eat/after-make-console-frame-hooks '()
  "Hooks to run after creating a new TTY frame")
(defvar eat/after-make-window-system-frame-hooks '()
  "Hooks to run after creating a new window-system frame")

(defun eat/run-after-make-frame-hooks (frame)
  "Run configured hooks in response to the newly-created FRAME.
Selectively runs either `eat/after-make-console-frame-hooks' or
`eat/after-make-window-system-frame-hooks'"
  (with-selected-frame frame
    (run-hooks (if window-system
                   'eat/after-make-window-system-frame-hooks
                 'eat/after-make-console-frame-hooks))))

(add-hook 'after-make-frame-functions 'eat/run-after-make-frame-hooks)

(defconst eat/initial-frame (selected-frame)
  "The frame (if any) active during Emacs initialization.")

(add-hook 'after-init-hook
          (lambda () (when eat/initial-frame
                       (eat/run-after-make-frame-hooks eat/initial-frame))))


;;; Theme
;;;; Variables

(defvar eat/theme 'modus-operandi
  "Default theme.")

(defvar eat/theme-tui 'default
  "Default theme used in tui.")

(defvar eat/theme-system-light 'modus-operandi
  "Default light theme after system appearance changed.")

(defvar eat/theme-system-dark 'modus-vivendi
  "Default dark theme after system appearance changed.")

(defvar luna-load-theme-hook nil)

;;;; Functions

(defun eat/load-theme (theme)
  (interactive
   (list
    (intern (completing-read "Theme: "
                             (mapcar #'symbol-name
				                     (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
  (if (featurep (intern (format "%s-theme" theme)))
      ;; We can save a lot of time by only enabling the theme.
      (enable-theme theme)
    (load-theme theme t))
  (run-hooks 'luna-load-theme-hook))
(global-set-key [remap load-theme] #'eat/load-theme)

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

(add-hook 'eat/after-make-console-frame-hooks (lambda ()
                                                (when (fboundp 'menu-bar-mode)
                                                  (menu-bar-mode -1))
                                                (when eat/theme-tui
                                                  (eat/load-theme eat/theme-tui))))

(add-hook 'eat/after-make-window-system-frame-hooks (lambda ()
                                                      (eat/load-theme eat/theme)))

;;; Font

(defvar luna-font-settings nil
  "A list of (FACE . FONT-NAME).
FONT-NAMEs are keys in ‘luna-font-alist’.")

(defvar luna-cjk-rescale-alist
  '(("Source Han Serif SC" . 1.3)
    ;; ("Source Han Sans SC" . 1.3)
    ("FZFW ZhuZi MinchoS" . 1.3))
  "A list of font names that should be rescaled.")

(defvar luna-font-alist
  `(("SF Mono" . ("SF Mono" "Source Han Serif SC" 1.3))
    ("IBM Plex Mono" . ("IBM Plex Mono" "Source Han Serif SC" 1.2))
    ("SF Pro Text" . ("SF Pro Text" "Source Han Serif SC" 1.1))
    ("IBM Plex Sans" . ("IBM Plex Sans" "Source Han Serif SC" 1.1))
    ("Dossier" . ("Dossier" "Source Han Serif SC" 1.3))
    ("Academica" . ("Academica Book" "Source Han Serif SC" 1.3))

    ;; TODO
    ("Bookerly" . ("Bookerly" "Source Han Serif SC" 1.3))
    ("Cascadia Code" . ("Cascadia Code" "Source Han Serif SC" 1.3))
    ("Monego" . ("Monego" "Source Han Serif SC" 1.3))
    ("Latin Modern Mono" . ("Latin Modern Mono" "Source Han Serif SC" 1.3))
    ("Menlo" . ("Menlo" "Source Han Serif SC" 1.3))
    ("MonoLisa Nasy" . ("MonoLisa Nasy" "Source Han Serif SC" 1.3))

    ("方正fW筑紫明朝" . (nil "FZFW ZhuZi MinchoS" 1))
    ("Source Han Serif" . (nil "Source Han Serif SC" 1))
    ("Source Han Sans" . (nil "Source Han Sans SC" 1))
    ("LXGW WenKai" . (nil "LXGW WenKai" 1))

    ("Charter 13" . ("Charter" nil 1 :size 13))
    ("GNU Unifont 15" . ("Unifont" nil 1 :size 15))
    ("SF Mono Light 13" . ("SF Mono" nil 1 :size 13 :weight light))
    ("PragmataPro 13" . ("PragmataPro Mono" nil 1 :size 13))
    ("Iosevka 13" . ("Iosevka" nil :size 14))
    ("JetBrains Mono 12" . ("JetBrains Mono" nil 1 :size 12))
    ("Roboto Mono 12" . ("Roboto Mono" nil 1 :size 12 :weight light)))
  "An alist of all the fonts you can switch between by `luna-load-font'.
Each element is like

    (FONT-NAME . (ASCII-NAME CJK-NAME CJK-SCALE))

FONT-NAME is the display name, ASCII-NAME is the ASCII font
family name, CJK-NAME is the CJK font family name, CJK-SCALE is
the CJK font rescale ratio.")

(defun luna-create-fontset (ascii-spec cjk-spec)
  "Create a fontset NAME with ASCII-SPEC and CJK-SPEC font."
  (let* ((fontset-name
          (concat "fontset-" (downcase (plist-get ascii-spec :family))))
         ;; ASCII font.
         (fontset
          (create-fontset-from-fontset-spec
           (font-xlfd-name
            (apply #'font-spec :registry fontset-name ascii-spec)))))
    ;; CJK font.
    (dolist (charset '(kana han cjk-misc))
      (set-fontset-font fontset charset (apply #'font-spec cjk-spec)))
    fontset))

(defun luna-font-name-to-spec (font-name size &rest attrs)
  "Translate FONT-NAME, SIZE and ATTRS to (ASCII-SPEC CJK-SPEC)."
  (let* ((font-spec (if (null font-name)
                        (cdar luna-font-alist)
                      (alist-get font-name luna-font-alist
                                 nil nil #'equal)))
         (ascii-family (nth 0 font-spec))
         (cjk-family (nth 1 font-spec))
         (cjk-scale (nth 2 font-spec))
         (rest-spec (append (nthcdr 3 font-spec) attrs))
         ;; (rest-spec (setf (plist-get rest-spec :size) size))
         (ascii-rest-spec (append `(:size ,size) rest-spec))
         (cjk-rest-spec (append `(:size ,(* cjk-scale size))
                                rest-spec))
         (ascii-spec (and ascii-family
                          `(:family ,ascii-family ,@ascii-rest-spec)))
         (cjk-spec (and cjk-family
                        `(:family ,cjk-family ,@cjk-rest-spec))))
    (list ascii-spec cjk-spec)))

(defun luna-load-default-font (font-name size &rest attrs)
  "Set font for default face to FONT-NAME with SIZE and ATTRS.
See ‘luna-load-font’."
  ;; We use a separate function for default font because Emacs has a
  ;; bug that prevents us from setting a fontset for the default face
  ;; (although ‘set-frame-parameter’ works). So we just set default
  ;; face with ASCII font and use default fontset for Unicode font.
  (interactive
   (list (completing-read
          "Font: " (mapcar #'car luna-font-alist))
         (string-to-number (completing-read
                            "Size: " nil nil nil nil nil "13"))))
  (let* ((specs (apply #'luna-font-name-to-spec font-name size attrs))
         (ascii (apply #'font-spec (car specs)))
         (cjk (apply #'font-spec (cadr specs))))
    (set-face-attribute 'default nil :font ascii)
    (set-fontset-font t 'kana cjk)
    (set-fontset-font t 'han cjk)
    (set-fontset-font t 'cjk-misc cjk)
    (set-fontset-font t 'symbol cjk nil 'append)))

(defun luna-load-font (face font-name size &rest attrs)
  "Set font for FACE to FONT-NAME.
If FONT-NAME is nil, use the first font in ‘luna-font-alist’.
SIZE is the font size in pt. Add additional face attributes in
ATTRS.

Use ‘luna-save-font-settings’ to save font settings and use
‘luna-load-saved-font’ to load them next time."
  (interactive
   (list (intern (completing-read
                  "Face: " (face-list)))
         (completing-read
          "Font: " (mapcar #'car luna-font-alist))
         (string-to-number (completing-read
                            "Size: " nil nil nil nil nil "13"))))
  (if (and (eq face 'default))
      (apply #'luna-load-default-font font-name size attrs)
    (let* ((fontset
            (apply #'luna-create-fontset
                   (apply #'luna-font-name-to-spec font-name size attrs))))
      (apply #'set-face-attribute face nil
             :font fontset
             :fontset fontset
             attrs)))
  ;; Save the settings.
  (setf (alist-get face luna-font-settings) `(,font-name ,size ,@attrs))
  (custom-set-variables
   `(luna-font-settings
	 ',luna-font-settings
	 nil nil "Automatically saved by ‘luna-load-font’")))

(defun luna-save-font-settings ()
  "Save font-settings set by ‘luna-load-font’."
  (interactive)
  (custom-save-all))

(defun luna-load-saved-font ()
  "Load font settings saved in ‘luna-font-settings’."
  (dolist (setting luna-font-settings)
	(apply #'luna-load-font setting)))

(define-minor-mode luna-scale-cjk-mode
  "Scale CJK font to align CJK font and ASCII font."
  :lighter ""
  :global t
  :group 'luna
  (dolist (setting luna-cjk-rescale-alist)
	(setf (alist-get (car setting)
                     face-font-rescale-alist nil nil #'equal)
		  (if luna-scale-cjk-mode (cdr setting) nil))))

(defun luna-enable-apple-emoji ()
  "Enable Apple emoji display."
  (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji")
                    nil 'prepend))

(defvar eat/font-default '("IBM Plex Mono" 13))
(defvar eat/font-variable-pitch '("Bookerly" 16))
(defvar eat/font-mode-line '("SF Pro Text" 13))
(defvar eat/font-emoji "Noto Color Emoji")

(add-hook 'eat/after-make-window-system-frame-hooks
          (lambda ()
            (set-fontset-font t 'emoji (font-spec :family eat/font-emoji) nil 'prepend)
            (luna-load-font 'default (car eat/font-default) (cadr eat/font-default) :weight 'medium)
            (luna-load-font 'fixed-pitch (car eat/font-default) (cadr eat/font-default) :weight 'medium)
            ;; (luna-load-font 'variable-pitch "Academica" 16)
            (luna-load-font 'variable-pitch (car eat/font-variable-pitch) (cadr eat/font-variable-pitch))
            (luna-load-font 'mode-line (car eat/font-mode-line) (cadr eat/font-mode-line) :weight 'light)
            (add-hook 'luna-load-theme-hook
                      (lambda ()
                        (luna-load-font 'mode-line "SF Pro Text" 13 :weight 'light)
                        (luna-load-font 'mode-line-inactive "SF Pro Text" 13 :weight 'light)))))


;;; mode-line
(defun luna-mode-line-with-padding (text)
  "Return TEXT with padding on the left.
The padding pushes TEXT to the right edge of the mode-line."
  (if (and (>= emacs-major-version 29) (display-graphic-p))
      (let* ((len (string-pixel-width text))
             (space-prop
              `(space :align-to (- (+ right right-margin) (,len))))
             (padding (propertize "-" 'display space-prop)))
        (concat padding text))
    (concat " " text)))

(defun luna-mode-line-coding-system ()
  "Display abnormal coding systems."
  (let ((coding (symbol-name buffer-file-coding-system)))
    (if (or (and (not (string-prefix-p "prefer-utf-8" coding))
                 (not (string-prefix-p "utf-8" coding))
                 (not (string-prefix-p "undecided" coding)))
            (string-suffix-p "dos" coding))
        (concat "  " coding)
      "")))

(setq-default mode-line-format
              (let* ((spaces
                      (propertize " " 'display '(space :width 1.5)))
                     (fringe (propertize
                              " " 'display '(space :width fringe)))
                     (percentage
                      '(format
                        "[%%l] %d%%"
                        (/ (* (window-end) 100.0) (point-max)))))
                `(,fringe
                  (:eval (if (window-dedicated-p) "🚷" ""))
                  (:eval (if buffer-read-only "🔒" ""))
                  (:propertize "%[%b%]" face (:weight bold))
                  (:eval (luna-mode-line-coding-system))
                  ,spaces
                  ,(propertize " " 'display '(raise 0.3))
                  ,(if (featurep 'minions)
                       'minions-mode-line-modes
                     'mode-line-modes)
                  ,(propertize " " 'display '(raise -0.3))
                  ,spaces
                  (:eval (if (buffer-modified-p)
                             ,(if (display-graphic-p) "ΦAΦ" "OAO")
                           ,(if (display-graphic-p) "ΦωΦ" "OwO")))
                  ,spaces
                  mode-line-misc-info
                  (:eval (concat (luna-mode-line-with-padding ,percentage)
                                 "%%"))
                  ;; (:eval (concat ,spaces "(%l) " ,percentage "%%"))
                  )))



;;; Optimization
(setq package-enable-at-startup nil
      frame-inhibit-implied-resize t
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      default-frame-alist '((scroll-bar-mode . 0)
                            (vertical-scroll-bars . nil)
                            (menu-bar-lines . 0)
                            (tool-bar-lines . 0)))

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

;; Speed up startup
(setq auto-mode-case-fold nil)

;; Optimization
(setq idle-update-delay 1.0)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; The nano style for truncated long lines.
(setq auto-hscroll-mode 'current-line)

(when eat/emacs29p
  ;; for mouse scroll
  (setq dired-mouse-drag-files t
        mouse-drag-and-drop-region t
        mouse-drag-and-drop-region-cross-program t)
  (add-hook 'after-init-hook (lambda () (pixel-scroll-precision-mode))))

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

;; Contrary to what many Emacs users have in their configs, you don't need
;; more than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")

;; Disable cursor blink
(add-hook 'after-init-hook (lambda () (blink-cursor-mode -1)))

;; Do not show cursor in nonselected windows
(setq-default cursor-in-non-selected-windows nil)

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
(setq ;; window-resize-pixelwise nil ;; NOTE this cause lsp-bridge-ref buffer didn't show
 frame-resize-pixelwise t)

;; Shut up!
(defun display-startup-echo-area-message()
  (message nil))

;; indent with whitespace by default
(setq-default
 tab-width 4
 indent-tabs-mode nil)

;; Disable default auto backup and save file
(setq-default
 create-lockfiles nil                               ; Don't create lockfiles
 make-backup-files nil                              ; Disable auto save and backup
 auto-save-default nil
 auto-save-list-file-prefix nil)

(setq
 initial-scratch-message (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n")
 initial-major-mode 'fundamental-mode               ; Don't use prog-mode an stratup
 ring-bell-function 'ignore
 read-process-output-max (* 4 1024 1024)
 suggest-key-bindings nil                           ; Disable "You can run the command balabala..."
 word-wrap-by-category t                            ; Emacs 之光！
 use-short-answers t                                ; yse-or-no -> y-or-n
 suggest-key-bindings nil
 enable-recursive-minibuffers t
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


;;; Setup PATH
;; https://emacs-china.org/t/emacs-mac-port-profile/2895/29?u=rua
;; NOTE: When PATH is changed, run the following command
;; $ sh -c 'printf "%s" "$PATH"' > ~/.path
(defun eat/getenv-path()
  (interactive)
  (condition-case err
      (let ((path (with-temp-buffer
                    (insert-file-contents-literally "~/.path")
                    (buffer-string))))
        (setenv "PATH" path)
        (setq exec-path (append (parse-colon-path path) (list exec-directory))))
    (error (warn "%s" (error-message-string err)))))


;;; Mac specific configuration
(when eat/macp
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        ;; Render thinner fonts
        ns-use-thin-smoothing t
        ;; Don't open a file in a new frame
        ns-pop-up-frames nil)

  (add-hook 'after-init-hook #'eat/getenv-path)

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
  (global-set-key [(super z)] #'undo)
  ;; `save-buffers-kill-emacs' will shutdown emacs daemon
  (global-set-key [(super q)] #'save-buffers-kill-terminal))


;;; Linux specific configuration
(when eat/linuxp
  (setq x-underline-at-descent-line t)
  (setq-default
   ;; Don't use Fcitx5 in Emacs in PGTK build
   pgtk-use-im-context-on-new-connection nil
   x-gtk-resize-child-frames nil)

  ;; Don't use GTK+ tooltip
  (when (boundp 'x-gtk-use-system-tooltips)
    (setq x-gtk-use-system-tooltips nil)))


;;; HACK Dvorak
;; Make “C-t” act like “C-x”, so it's easier to type on Dvorak layout
(keyboard-translate ?\C-t ?\C-x)
(keyboard-translate ?\C-x ?\C-t)
;; use C-u to forward, the origin C-u map to C-c C-u
(global-set-key (kbd "C-u") #'forward-char)
(global-set-key (kbd "C-x C-u") #'universal-argument)


;;; Keybindings
;; bind `describe-keymap', added in emacs 28
(global-set-key (kbd "C-h C-k") #'describe-keymap)

;; this will stuck emacs
(global-unset-key (kbd "C-h h"))

;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(global-set-key [remap move-beginning-of-line] #'smarter-move-beginning-of-line)


;;; Built-in packages
(eat-package pulse
  :hook
  ((imenu-after-jump-hook isearch-update-post-hook)
   . eat/recenter-and-pulse)
  ((bookmark-after-jump  next-error)
   . eat/recenter-and-pulse-line)
  :init
  (custom-set-faces
   '(pulse-highlight-start-face ((t (:inherit region))))
   '(pulse-highlight-face ((t (:inherit region)))))

  (defun eat/pulse-momentary-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))

  (defun eat/pulse-momentary (&rest _)
    "Pulse the region or the current line."
    (if (fboundp 'xref-pulse-momentarily)
        (xref-pulse-momentarily)
      (eat/pulse-momentary-line)))

  (defun eat/recenter-and-pulse(&rest _)
    "Recenter and pulse the region or the current line."
    (recenter)
    (eat/pulse-momentary))

  (defun eat/recenter-and-pulse-line (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (eat/pulse-momentary-line))

  (dolist (cmd '(recenter-top-bottom
                 other-window windmove-do-window-select
                 pager-page-down pager-page-up))
    (advice-add cmd :after #'eat/pulse-momentary-line))

  (dolist (cmd '(pop-to-mark-command
                 pop-global-mark
                 goto-last-change))
    (advice-add cmd :after #'eat/recenter-and-pulse)))

(eat-package repeat
  :init
  (setq repeat-mode t
        repeat-keep-prefix t
        repeat-exit-timeout 3
        repeat-exit-key (kbd "RET")))

(when eat/emacs29p
  ;; TODO use with hideshow
  (eat-package mouse
    :hook (after-init-hook . context-menu-mode)))

(eat-package minibuffer
  :init
  (setq
   completion-styles '(basic partial-completion)
   completion-category-overrides '((file (styles basic partial-completion)))
   completion-cycle-threshold t
   minibuffer-depth-indicate-mode t
   minibuffer-eldef-shorten-default t
   minibuffer-electric-default-mode t))

(eat-package display-line-numbers
  :init
  (setq display-line-numbers-width 3))

(eat-package subword
  :hook (prog-mode-hook . subword-mode))

(eat-package simple
  :hook (before-save-hook . delete-trailing-whitespace)
  :init
  (setq visual-line-fringe-indicators '(nil right-curly-arrow)
        ;; List only applicable commands.
        read-extended-command-predicate #'command-completion-default-include-p
        fill-column 72))

(eat-package hl-line
  :init
  (setq-default hl-line-sticky-flag nil)
  ;; (when (display-graphic-p)
  ;;   (add-hook 'prog-mode-hook #'hl-line-mode)
  ;;   (add-hook 'conf-mode-hook #'hl-line-mode))
  :config
  (add-hook 'post-command-hook #'(lambda ()
                                   "When `hl-line-mode' is enable, unhighlight if region is active."
                                   (when (and (bound-and-true-p hl-line-mode)
                                              (region-active-p))
                                     (hl-line-unhighlight)))))

(eat-package tramp
  :init
  (setq
   tramp-verbose 1 ;; only show error message
   tramp-completion-reread-directory-timeout nil ;;  speed up complete
   tramp-auto-save-directory temporary-file-directory
   ;; Always use file cache when using tramp
   remote-file-name-inhibit-cache nil
   ;; C-x C-f /ssh:
   tramp-default-method "ssh"
   vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                vc-ignore-dir-regexp
                                tramp-file-name-regexp))

  (defun eat/reopen-file-with-sudo ()
    (interactive)
    (find-alternate-file (format "/sudo::%s" (buffer-file-name))))
  (global-set-key (kbd "C-x C-z") #'eat/reopen-file-with-sudo)
  :config
  ;; use `magit' with yadm, (magit-status "/yadm::")
  (add-to-list 'tramp-methods
               '("yadm"
                 (tramp-login-program "yadm")
                 (tramp-login-args (("enter")))
                 (tramp-login-env (("SHELL") ("/bin/sh")))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-args ("-c"))))
  ;; ‘Private Directories’ are the settings of the $PATH environment,
  ;; as given in your ‘~/.profile’.  This entry is represented in
  ;; the list by the special value ‘tramp-own-remote-path’.
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(eat-package isearch
  :init
  (setq
   ;; Match count next to the minibuffer prompt
   isearch-lazy-count t
   ;; Don't be stingy with history; default is to keep just 16 entries
   search-ring-max 200
   regexp-search-ring-max 200
   ;; htighlighted all matching
   isearch-lazy-highlight t
   lazy-highlight-buffer t
   ;; show search count, TODO not work in isearch-mb-mode
   lazy-count-prefix-format nil
   lazy-count-suffix-format " [%s/%s]"
   ;; Record isearch in minibuffer history, so C-x ESC ESC can repeat it.
   isearch-resume-in-command-history t
   ;; M-< and M-> move to the first/last occurrence of the current search string.
   isearch-allow-motion t
   isearch-motion-changes-direction t
   ;; space matches any sequence of characters in a line.
   isearch-regexp-lax-whitespace t
   search-whitespace-regexp ".*?")
  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "C-r") 'isearch-backward-regexp)
  :config
  (define-advice isearch-occur (:after (_regexp &optional _nlines))
    (isearch-exit))
  (define-key isearch-mode-map (kbd "C-c C-o") #'isearch-occur)
  (define-key isearch-mode-map [escape] #'isearch-cancel)
  ;; Edit the search string instead of jumping back
  (define-key isearch-mode-map [remap isearch-delete-chac] #'isearch-del-chac))

(eat-package eldoc
  :init
  (setq eldoc-idle-delay 1))

(eat-package whitespace
  :hook
  ((prog-mode-hook conf-mode-hook) . whitespace-mode)
  :init
  (setq whitespace-style '(face trailing)))

(eat-package hideshow
  :hook (prog-mode-hook . hs-minor-mode)
  :init
  ;; FIXME
  (defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))

  (defface hideshow-border-face
    '((((background light))
       :background "rosy brown" :extend t)
      (t
       :background "sandy brown" :extend t))
    "Face used for hideshow fringe."
    :group 'hideshow)

  (define-fringe-bitmap 'hideshow-folded-fringe
    (vector #b00000000
            #b00000000
            #b00000000
            #b11000011
            #b11100111
            #b01111110
            #b00111100
            #b00011000))

  (defun hideshow-folded-overlay-fn (ov)
    "Display a folded region indicator with the number of folded lines."
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
             (info (format " (%d)..." nlines)))
        ;; fringe indicator
        (overlay-put ov 'before-string (propertize " "
                                                   'display '(left-fringe hideshow-folded-fringe
                                                                          hideshow-border-face)))
        ;; folding indicator
        (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))

  (setq hs-set-up-overlay #'hideshow-folded-overlay-fn))

(eat-package xref
  :hook
  ((xref-after-return-hook xref-after-jump-hook) . recenter)
  :init
  (global-unset-key (kbd "C-<down-mouse-1>"))
  (global-set-key (kbd "C-<mouse-1>") #'xref-find-definitions-at-mouse)
  (setq xref-prompt-for-identifier nil
        xref-search-program 'ripgrep
        xref-show-xrefs-function #'xref-show-definitions-completing-read
        xref-show-definitions-function #'xref-show-definitions-completing-read))

(eat-package dired
  :hook (dired-mode-hook . dired-hide-details-mode)
  :init
  (when eat/macp
    (setq insert-directory-program "gls"))
  (setq
   dired-dwim-target t
   dired-kill-when-opening-new-dired-buffer t
   dired-listing-switches
   "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
  :config
  (define-key dired-mode-map (kbd "h") #'dired-up-directory) ; remapped `describe-mode'
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "C-c C-p") #'wdired-change-to-wdired-mode))

(eat-package ibuffer
  :init
  (fset 'list-buffers 'ibuffer)
  (setq-default ibuffer-show-empty-filter-groups nil)
  (global-set-key (kbd "C-x B") 'ibuffer)
  ;; Modify the default ibuffer-formats (toggle with `)
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 22 22 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 12 12 :left :elide)
                " "
                vc-relative-file)
          (mark modified read-only vc-status-mini " "
                (name 22 22 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 14 14 :left :elide)
                " "
                (vc-status 12 12 :left)
                " "
                vc-relative-file)))

  (setq ibuffer-filter-group-name-face 'font-lock-doc-face)
  :config
  (fullframe ibuffer ibuffer-quit)
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size))))

(eat-package ediff
  :init
  (defvar local-ediff-saved-window-conf nil)

  (defun eat/ediff-save-window-conf ()
    (setq local-ediff-saved-window-conf (current-window-configuration)))

  (defun eat/ediff-restore-window-conf ()
    (when (window-configuration-p local-ediff-saved-window-conf)
      (set-window-configuration local-ediff-saved-window-conf)))

  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-highlight-all-diffs t
        ediff-split-window-function 'split-window-horizontally
        ediff-merge-split-window-function 'split-window-horizontally)
  :config
  ;; Restore window config after quitting ediff
  (add-hook 'ediff-before-setup-hook #'eat/ediff-save-window-conf)
  (add-hook 'ediff-quit-hook #'eat/ediff-restore-window-conf))

(eat-package ispell
  :init
  (when eat/macp
    (setenv "DICTIONARY" "en_US"))
  ;; no spell checking for org special blocks
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_src" . "#\\+end_src"))
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_example" . "#\\+end_example"))
  (setq ispell-really-hunspell t
        ispell-program-name "hunspell"
        ispell-dictionary "en_US"
        ispell-following-word t
        ispell-personal-dictionary (locate-user-emacs-file "hunspell_dict.txt")))

(eat-package flyspell
  :init
  ;; `flyspell' -- only enable in magit commit
  (setq flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil)
  :config
  (setq flyspell-mode-map nil))

(eat-package project
  :init
  (defun eat/project-name ()
    (file-name-nondirectory
     (directory-file-name
      (project-root
       (project-current)))))

  ;; do not remember tramp project
  (defun eat/project-remember-advice (fn pr &optional no-write)
    (let* ((remote? (file-remote-p (project-root pr)))
           (no-write (if remote? t no-write)))
      (funcall fn pr no-write)))
  (advice-add 'project-remember-project :around
              'eat/project-remember-advice)

  :config
  (defun eat/project-files-in-directory (dir)
    "Use `fd' to list files in DIR."
    (let* ((default-directory dir)
           (localdir (file-local-name (expand-file-name dir)))
           (command (format "fd -c never -H -t f -0 . %s" localdir)))
      (project--remote-file-names
       (sort (split-string (shell-command-to-string command) "\0" t)
             #'string<))))

  ;; use fd in `project-find-file'
  (when (executable-find "fd")
    (cl-defmethod project-files ((project (head local)) &optional dirs)
      "Override `project-files' to use `fd' in local projects."
      (mapcan #'eat/project-files-in-directory
              (or dirs (list (project-root project))))))

  (defun eat/project-try-local (dir)
    "Determine if DIR is a non-Git project."
    (catch 'ret
      (let ((pr-flags '((".project")
                        ("go.mod" "Cargo.toml" "project.clj" "pom.xml" "package.json") ;; higher priority
                        ("Makefile" "README.org" "README.md"))))
        (dolist (current-level pr-flags)
          (dolist (f current-level)
            (when-let ((root (locate-dominating-file dir f)))
              (throw 'ret (cons 'local root))))))))
  (cl-defmethod project-root ((project (head local)))
    (cdr project))
  (add-to-list 'project-find-functions #'eat/project-try-local t))

(eat-package tab-bar
  :init
  (setq tab-bar-border nil
        tab-bar-close-button nil
        tab-bar-back-button nil
        tab-bar-new-button nil
        tab-bar-show nil
        tab-bar-format '(tab-bar-format-tabs)
        tab-bar-tab-name-format-function 'eat/tab-bar-tab-format-function
        tab-bar-separator ""
        tab-bar-tab-name-truncated-max 10)

  (defun eat/tab-bar-switch-project ()
    "Switch to project in a new tab, project name will be used as tab name.

No tab will created if the command is cancelled."
    (interactive)
    (let (succ)
      (unwind-protect
          (progn
            (tab-bar-new-tab)
            (call-interactively #'project-switch-project)
            (when-let ((proj (project-root (project-current))))
              (tab-bar-rename-tab (format "%s" (file-name-nondirectory (directory-file-name proj))))
              (setq succ t)))
        (unless succ
          (tab-bar-close-tab)))))

  (defun eat/tab-bar-tab-format-function (tab i)
    (let ((current-p (eq (car tab) 'current-tab)))
      (propertize (concat
                   "   "
                   (alist-get 'name tab)
                   "   ")
                  'face
                  (funcall tab-bar-tab-face-function tab))))
  :config
  (define-key tab-prefix-map (kbd ".") #'tab-bar-switch-to-recent-tab)
  (define-key tab-prefix-map (kbd ",") #'tab-bar-rename-tab)
  (define-key tab-prefix-map (kbd "l") #'eat/tab-bar-switch-project))

(eat-package paren
  :init
  (setq show-paren-when-point-in-periphery t
        show-paren-context-when-offscreen 'overlay
        show-paren-when-point-inside-paren t)
  (when eat/emacs29p
    (setq show-paren-context-when-offscreen t)))

(eat-package elec-pair
  :hook (prog-mode-hook . electric-pair-local-mode)
  :init
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(eat-package smerge-mode
  :hook (find-file-hook . (lambda ()
                            (save-excursion
                              (goto-char (point-min))
                              (when (re-search-forward "^<<<<<<< " nil t)
                                (smerge-mode 1)))))
  :config
  (define-key smerge-mode-map (kbd "C-c r") #'smerge-refine)
  (define-key smerge-mode-map (kbd "C-c c") #'smerge-keep-current)
  (define-key smerge-mode-map (kbd "C-c a") #'smerge-keep-all)
  (define-key smerge-mode-map (kbd "C-c n") #'smerge-next)
  (define-key smerge-mode-map (kbd "C-c p") #'smerge-prev)
  (define-key smerge-mode-map (kbd "C-c l") #'smerge-keep-lower)
  (define-key smerge-mode-map (kbd "C-c u") #'smerge-keep-upper))

(eat-package cc-mode
  :init
  (setq c-default-style "linux"
        c-basic-offset 4))

(eat-package python
  :init
  (setq python-indent-offset 4
        python-shell-completion-native-enable nil
        python-shell-interpreter "ipython"
        python-indent-guess-indent-offset nil))

(eat-package sql
  :init
  (setq sql-mysql-login-params '(user password server database port)))

(eat-package xwidget
  :init
  ;; use `eyebrowse' replace `tab-bar'
  ;; run `xwidget-webkit-browse-url' in other tab
  ;; (advice-add 'xwidget-webkit-browse-url :before #'(lambda (url &optional new-session)
  ;;                                                    "Run `xwidget-webkit-browse-url' in name tab 'xwidget'."
  ;;                                                    (tab-bar-select-tab-by-name "xwidget")))
  :config
  (define-key xwidget-webkit-mode-map (kbd "y") #'xwidget-webkit-copy-selection-as-kill))

(eat-package webjump
  :init
  (global-set-key (kbd "C-x C-/") #'webjump)
  (setq webjump-sites
        '(("Emacs Wiki" .
           [simple-query "www.emacswiki.org" "www.emacswiki.org/cgi-bin/wiki/" #1=""])
          ("Emacs China" . "emacs-china.org")
          ("Emacs Reddit" . "www.reddit.com/r/emacs/")
          ("Emacs News" . "sachachua.com/blog/category/emacs-news/")
          ("Github" .
           [simple-query "github.com" "github.com/search?q=" #1#])
          ("DuckDuckGo" .
           [simple-query "duckduckgo.com" "duckduckgo.com/?q=" #1#])
          ("Google" .
           [simple-query "google.com" "google.com/search?q=" #1#])
          ("Youtube" .
           [simple-query "youtube.com" "youtube.com/results?search_query=" #1#])
          ("Google Groups" .
           [simple-query "groups.google.com" "groups.google.com/groups?q=" #1#])
          ("stackoverflow" .
           [simple-query "stackoverflow.com" "stackoverflow.com/search?q=" #1#])
          ("Wikipedia" .
           [simple-query "wikipedia.org" "wikipedia.org/wiki/" #1#]))))

(eat-package outline
  :init
  (setq outline-minor-mode-cycle t
        outline-minor-mode-highlight t))

(eat-package newcomment
  :init
  (setq comment-auto-fill-only-comments t))

(eat-package message
  :hook (message-mode-hook . (lambda ()
                               (setq-local fill-column 72)
                               (auto-fill-mode)))
  :init
  (setq user-full-name eat/user-full-name
        user-mail-address eat/user-mail-address
        message-kill-buffer-on-exit t
        message-mail-alias-type 'ecomplete
        message-send-mail-function #'message-use-send-mail-function
        message-signature user-full-name))

(eat-package sendmail
  :init
  (setq send-mail-function #'smtpmail-send-it))

(eat-package smtpmail
  :init
  (setq smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-user user-mail-address
        smtpmail-smtp-service 587
        smptmail-stream-type 'ssl))

(eat-package flymake
  :hook
  (prog-mode-hook . flymake-mode)
  (emacs-lisp-mode-hook . (lambda ()
                            (flymake-mode -1)))
  :init
  (setq-default flymake-diagnostic-functions nil
                flymake-no-changes-timeout 0.2)
  (defun sekiro-flymake-mode-line-format ()
    (let* ((counter (string-to-number
                     (nth 1
                          (cadr
                           (flymake--mode-line-counter :error t)))))
           (sekiro-flymake (when (> counter 0)
                             'compilation-error)))
      (propertize
       "危"
       'face
       sekiro-flymake))))

(eat-package so-long
  :hook (after-init-hook . global-so-long-mode))

(eat-package autorevert
  :hook (after-init-hook . global-auto-revert-mode))

(eat-package saveplace
  :hook (after-init-hook . save-place-mode))

(eat-package winner
  :hook (after-init-hook . winner-mode)
  :init
  (setq winner-dont-bind-my-keys t))

(eat-package savehist
  :hook (after-init-hook . savehist-mode)
  :init
  ;; Restore histories and registers after saving
  (setq history-length 1000))

(defun term-mode-common-init ()
  "The common initialization procedure for term/shell."
  (setq-local scroll-margin 0)
  (setq-local truncate-lines t)
  (setq-local global-hl-line-mode nil))

(eat-package term
  :hook
  (term-mode-hook . (term-mode-prompt-regexp-setup term-mode-common-init))
  :init
  (defun term-mode-prompt-regexp-setup ()
    "Setup `term-prompt-regexp' for term-mode."
    (setq-local term-prompt-regexp "^[^#$%>\n]*[#$%>] *")))

(eat-package eshell
  :hook
  (eshell-mode-hook . (lambda ()
                        (term-mode-common-init)
                        ;; Eshell is not fully functional
                        (setenv "PAGER" "cat")))
  :config
  ;; Prevent accident typing
  (defalias 'eshell/vi 'find-file)
  (defalias 'eshell/vim 'find-file)

  (defun eshell/bat (file)
    "cat FILE with syntax highlight."
    (with-temp-buffer
      (insert-file-contents file)
      (let ((buffer-file-name file))
        (delay-mode-hooks
          (set-auto-mode)
          (font-lock-ensure)))
      (buffer-string)))

  (defun eshell/f (filename &optional dir)
    "Search for files matching FILENAME in either DIR or the
current directory."
    (let ((cmd (concat
                (executable-find "find")
                " " (or dir ".")
                "      -not -path '*/.git*'"
                " -and -not -path 'build'"    ;; the cmake build directory
                " -and"
                " -type f"
                " -and"
                " -iname '*" filename "*'")))
      (eshell-command-result cmd)))

  (defun eshell/z ()
    "cd to directory with completion."
    (let ((dir (completing-read "Directory: " (ring-elements eshell-last-dir-ring) nil t)))
      (eshell/cd dir)))

  (defun eshell-prompt ()
    "Prompt for eshell."
    (concat
     (propertize user-login-name 'face 'font-lock-keyword-face)
     "@"
     "Noobmaster "
     (if (equal (eshell/pwd) "~")
         "~"
       (abbreviate-file-name (eshell/pwd)))
     " "
     (if-let* ((vc (ignore-errors (vc-responsible-backend default-directory)))
               (br (car (vc-git-branches))))
         (concat (propertize "(" 'face 'success)
                 (format "%s" vc)
                 (propertize ")" 'face 'success)
                 (propertize "-" 'face 'font-lock-string-face)
                 (propertize "[" 'face 'success)
                 (propertize br 'face 'font-lock-constant-face)
                 (propertize "]" 'face 'success)
                 " ")
       "")
     "% ")))

;; The interactive shell.
;;
;; It can be used as a `sh-mode' REPL.
;;
;; `shell' is recommended to use over `tramp'.
(eat-package shell
  :hook
  (shell-mode-hook . (term-mode-common-init revert-tab-width-to-default))
  :init
  (defun shell-toggle ()
    "Toggle a persistent shell popup window.
If popup is visible but unselected, select it.
If popup is focused, kill it."
    (interactive)
    (if-let ((win (get-buffer-window "*shell-popup*")))
        (when (eq (selected-window) win)
          ;; If users attempt to delete the sole ordinary window, silence it.
          (ignore-errors (delete-window win)))
      (let ((display-comint-buffer-action '(display-buffer-at-bottom
                                            (inhibit-same-window . nil))))

        (shell "*shell-popup*"))))

  ;; Correct indentation for `ls'
  (defun revert-tab-width-to-default ()
    "Revert `tab-width' to default value."
    (setq-local tab-width 8)))

;; Quick editing in `describe-variable'
(with-eval-after-load 'help-fns
  (put 'help-fns-edit-variable 'disabled nil))


;;; init-eat.el ends here
(provide 'init-eat)
