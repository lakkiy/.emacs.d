;;; init-builtin.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Variables
;;
;; NOTE Custom these in pre-init.el
(defvar my/fonts-default '("Monaco" "Cascadia Code" "Menlo" "Source Code Pro")
  "List of fonts to try when setting the default font.")

(defvar my/fonts-variable-pitch '("Bookerly" "Cardo" "Times New Roman" "DejaVu Sans")
  "List of fonts to try when setting the variable-pitch font.")

(defvar my/fonts-cjk '("LXGW WenKai" "FZYouSong GBK" "WenQuanYi Micro Hei" "Microsoft Yahei")
  "List of fonts to try when setting the CJK font.")

(defvar my/fonts-unicode '("Symbola")
  "List of fonts to try when setting the Unicode font.")

(defvar my/fonts-emoji '("Apple Color Emoji" "Segoe UI Symbol" "Noto Color Emoji")
  "List of fonts to try when setting the Emoji font.")

(defvar my/font-size-default 12
  "Default font size.")

(defvar my/theme 'modus-operandi
  "The default theme.")

(defvar my/theme-tui 'modus-vivendi
  "The default theme for TUI.")

(defvar after-load-theme-hook nil
  "Hooks run after `load-theme'.")

;;; Disable stupid things
;;
;; Reduce *Message* noise at startup. An empty scratch buffer (or the
;; dashboard) is more than enough, and faster to display.
(setq inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-buffer-menu t
      inhibit-startup-echo-area-message user-login-name
      inhibit-x-resources t
      inhibit-default-init t
      server-client-instructions nil
      suggest-key-bindings nil
      x-gtk-use-system-tooltips nil
      x-gtk-use-native-input t
      x-gtk-resize-child-frames 'resize-mode)

;; Remove "For information about GNU Emacs..." message at startup
(fset #'display-startup-echo-area-message #'ignore)

;; Disable warnings from the legacy advice API. They aren't useful.
(setq ad-redefinition-action 'accept)

;; Ignore warnings about "existing variables being aliased".
(setq warning-suppress-types '((defvaralias) (lexical-binding)))

;; Disable GUIs because they are inconsistent across systems, desktop
;; environments, and themes, and they don't match the look of Emacs.
(setq use-file-dialog nil
      use-dialog-box nil)

;; No annoying bell
(setq ring-bell-function 'ignore)

;; No eyes distraction
(setq blink-cursor-mode nil)

;; Allow for shorter responses: "y" for yes and "n" for no.
(setq use-short-answers t
      read-answer-short t
      ;; 直接读取单个字符（如 y 或 n），不需要 enter 确认
      y-or-n-p-use-read-key t
      read-char-choice-use-read-key t)

;; Never show the hello file
(keymap-global-unset "C-h h")

;; Do not hide emacs
(keymap-global-unset "M-z")
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;;; Better default
;;
;; By default, Emacs "updates" its ui more often than it needs to
(setq idle-update-delay 1.0)

;; Ensures that empty lines within the commented region are also commented out.
;; This prevents unintended visual gaps and maintains a consistent appearance,
;; ensuring that comments apply uniformly to all lines, including those that are
;; otherwise empty.
(setq comment-empty-lines t)

;; We often split terminals and editor windows or place them side-by-side,
;; making use of the additional horizontal space.
(setq-default fill-column 80)

;; Position underlines at the descent line instead of the baseline.
(setq x-underline-at-descent-line t)

;; No Fcitx5 in Emacs PGTK build.
(setq pgtk-use-im-context-on-new-connection nil)

;; Better word wrapping for CJK characters
(setq word-wrap-by-category t)

;; Disable the obsolete practice of end-of-line spacing from the
;; typewriter era.
(setq sentence-end-double-space nil)

;; Echo current unfinished command immediately.
(setq echo-keystrokes 0.1)

;; Allow nested minibuffers
(setq enable-recursive-minibuffers t)

;; Keep the cursor out of the read-only portions of the.minibuffer
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Disable truncation of printed s-expressions in the message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;; Delete by moving to trash in interactive mode
(setq delete-by-moving-to-trash (not noninteractive))

;; Disable the warning "X and Y are the same file". Ignoring this warning is
;; acceptable since it will redirect you to the existing buffer regardless.
(setq find-file-suppress-same-file-warnings t)

;; Resolve symlinks when opening files, so that any operations are conducted
;; from the file's true directory (like `find-file').
(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; According to the POSIX, a line is defined as "a sequence of zero or
;; more non-newline characters followed by a terminating newline".
(setq require-final-newline t)

(global-set-key [remap keyboard-quit] 'keyboard-escape-quit)

(setq native-comp-async-report-warnings-errors 'silent)

;;; Performance
;;
;; Improve Emacs' responsiveness by delaying syntax highlighting during input
;; but may reduce visual feedback.
(setq redisplay-skip-fontification-on-input t)

;; Resizing the Emacs frame can be costly when changing the font. Disable this
;; to improve startup times with fonts larger than the system default.
(setq frame-resize-pixelwise t)
;; However, do not resize windows pixelwise, as this can cause crashes in some
;; cases when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)

;; Shave seconds off startup time by starting the scratch buffer in
;; `fundamental-mode'
(setq initial-buffer-choice nil
      initial-major-mode 'fundamental-mode)

;; Increase how much is read from processes in a single chunk (default is 4kb).
(setq read-process-output-max (* 4 1024 1024)
      process-adaptive-read-buffering nil)

;; Reduce rendering/line scan work by not rendering cursors or regions in
;; non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
;; No second pass of case-insensitive search over auto-mode-alist.
(setq auto-mode-case-fold nil)

;; Hide commands in M-x which do not work in the current mode, reduce command
;; completion overhead.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Long line
;;
;; https://emacs-china.org/t/topic/25811/9?u=rua
(setq long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)
(add-hook 'after-init-hook #'global-so-long-mode)

;; Disable bidirectional text scanning for a modest performance boost.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Give up some bidirectional functionality for slightly faster re-display.
(setq bidi-inhibit-bpa t)

;;; Modern editor behavior
;;;; Auto revert
;;
;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(setq revert-without-query (list ".")  ; Do not prompt
      auto-revert-stop-on-user-input nil
      ;; Revert other buffers (e.g, Dired)
      global-auto-revert-non-file-buffers t)

(add-hook 'after-init-hook #'global-auto-revert-mode)

;;;; Auto insert pair
;;
;; 自动插入/删除配对括号
;;
;; Not to pair when:
;; same char is next
;; next to a word
;; insert the second of "" or of ((
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)

(add-hook 'prog-mode-hook #'electric-pair-local-mode)

;;;; Auto save changed file
;;
;; But most of time I will do save manually.
(setq auto-save-visited-interval 10)

(add-hook 'after-init-hook #'auto-save-visited-mode)

;;;; Coding system
;;
;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

;; Contrary to what many Emacs users have in their configs, you don't need
;; more than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")
;; Set-language-environment sets default-input-method, which is unwanted.
(setq default-input-method nil)

;;;; Delete selection
(add-hook 'after-init-hook #'delete-selection-mode)

;;;; Disable auto-save/backup/lock files
;;
;; Avoid generating backups or lockfiles to prevent creating world-readable
;; copies of files.
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

;;;; Delete trailing whitespace after save file
(add-hook 'after-save-hook #'delete-trailing-whitespace)

;;;; Enable right click menu
(add-hook 'after-init-hook #'context-menu-mode)

;;;; Open recent file
;;
;; `recentf' is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(setq recentf-max-saved-items 1000 ;; default is 20
      recentf-auto-cleanup 'never
      recentf-exclude `(,tramp-file-name-regexp
                        "COMMIT_EDITMSG"
                        "^/\\(?:ssh\\|su\\|sudo\\)?:"))

(keymap-global-set "C-x C-r" #'recentf-open-files)

(add-hook 'after-init-hook #'(lambda()
                               (let ((inhibit-message t))
                                 (recentf-mode 1))))


;;;; Highlight url/mail address, click to go
;;
;; Also needed by `webpaste'
(setq browse-url-generic-program
      (or (when (eq system-type 'darwin) "open")
          (when (eq system-type 'gnu/linux) "xdg-open")))

(add-hook 'after-init-hook #'global-goto-address-mode)

;;;; Line number
(setq display-line-numbers-width 3)

;; 需要的时候手动开启
;; (add-hook 'after-init-hook #'display-line-numbers-mode)

;;;; Subword
(add-hook 'prog-mode-hook #'subword-mode)

;;;; Scrolling
;;
;; 每次滚动 1 行
(setq scroll-step 1)

;; 跳过部分语法高亮计算，提高滚动速度
(setq fast-but-imprecise-scrolling t)

;; 滚动超出缓冲区时，将光标移动到顶部或底部，而不是报错。
(setq scroll-error-top-bottom t)

;; 滚动时保持屏幕相对位置不变，避免屏幕跳动。
(setq scroll-preserve-screen-position t)

;; 光标移动超过窗口边缘 10 行时，才会重新居中窗口（默认为 0 直接重新居中）
(setq scroll-conservatively 10)

;; The nano style for truncated long lines.
(setq auto-hscroll-mode 'current-line)

;; 禁用自动垂直滚动，以解决编辑超长行时的光标滞后和屏幕跳动问题
(setq auto-window-vscroll nil)

;; Horizontal scrolling, use shift + mouse wheel to scrll horizontally.
(setq hscroll-margin 2
      hscroll-step 1
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

;; Smooth scroll up & down
(setq pixel-scroll-precision-interpolate-page t)
(add-hook 'after-init-hook #'pixel-scroll-precision-mode)

(defun pixel-scroll-down (&optional lines)
  (interactive)
  (if lines
      (pixel-scroll-precision-interpolate (* -1 lines (pixel-line-height)))
    (pixel-scroll-interpolate-down)))

(defun pixel-scroll-up (&optional lines)
  (interactive)
  (if lines
      (pixel-scroll-precision-interpolate (* lines (pixel-line-height))))
  (pixel-scroll-interpolate-up))

(defalias 'scroll-up-command 'pixel-scroll-interpolate-down)
(defalias 'scroll-down-command 'pixel-scroll-interpolate-up)

;;;; Save place
;;
;; `save-place-mode' enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(add-hook 'after-init-hook #'save-place-mode)

;;;; Show parenthesises
;;
;; Highlight parenthesises
(setq show-paren-context-when-offscreen 'overlay
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t
      show-paren-context-when-offscreen t)
(add-hook 'after-init-hook #'show-paren-mode)


;;;; Save minibuffer history
;;
;; `savehist-mode' is an Emacs feature that preserves the minibuffer history
;; between sessions. It saves the history of inputs in the minibuffer, such as
;; commands, search strings, and other prompts, to a file. This allows users to
;; retain their minibuffer history across Emacs restarts.
(setq history-length 1000)

(add-hook 'after-init-hook #'savehist-mode)

;;;; TAB
;;
;; Prefer spaces over tabs. Spaces offer a more consistent default compared to
;; 8-space tabs. This setting can be adjusted on a per-mode basis as needed.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; First indent current line, or try to complete if the line was already
;; indented.
(setq tab-always-indent 'complete)

;;; Find external programs
;;
;; Set PATH and `exec-path'
;; https://emacs-china.org/t/emacs-mac-port-profile/2895/29?u=rua
;; NOTE: When PATH is changed, run the following command
;; $ sh -c 'printf "%s" "$PATH"' > ~/.path
(defun my/getenv-path()
  (interactive)
  (condition-case err
      (let ((path (with-temp-buffer
                    (insert-file-contents-literally "~/.path")
                    (buffer-string))))
        (setenv "PATH" path)
        (setq exec-path (append (parse-colon-path path) (list exec-directory))))
    (error (warn "%s" (error-message-string err)))))

(when (file-exists-p "~/.path")
  (add-hook 'after-init-hook #'my/getenv-path))

;;; Font & Theme
(defun font-installed-p (font-list)
  (catch 'font-found
    (dolist (font font-list)
      (when (find-font (font-spec :name font))
        (throw 'font-found font)))))

(defun my/setup-font ()
  (let* ((my/font-default        (font-installed-p my/fonts-default))
         (my/font-variable-pitch (font-installed-p my/fonts-variable-pitch))
         (my/font-cjk            (font-installed-p my/fonts-cjk))
         (my/font-unicode        (font-installed-p my/fonts-unicode))
         (my/font-emoji          (font-installed-p my/fonts-emoji))
         (my/font-rescale-alist  `((,my/font-cjk     . 0.95)
                                   (,my/font-emoji   . 0.9)
                                   (,my/font-unicode . 0.95)
                                   (,my/font-variable-pitch . 1.2))))
    (set-face-attribute 'default nil :height (* 10 my/font-size-default))
    (when my/font-default
      (set-face-attribute 'default     nil :family my/font-default)
      (set-face-attribute 'fixed-pitch nil :font my/font-default))
    (when my/font-variable-pitch
      (set-face-font 'variable-pitch my/font-variable-pitch))
    (when my/font-unicode
      (set-fontset-font t 'unicode my/font-unicode))
    (when my/font-emoji
      (set-fontset-font t 'emoji   my/font-emoji))
    (when my/font-cjk
      (set-fontset-font t 'kana     my/font-cjk)
      (set-fontset-font t 'han      my/font-cjk)
      (set-fontset-font t 'cjk-misc my/font-cjk))
    (dolist (setting my/font-rescale-alist)
      (when (car setting)
        (setf (alist-get (car setting)
                         face-font-rescale-alist nil nil #'equal)
              (cdr setting))))))

(defun my/load-theme (f theme &optional no-confirm no-enable &rest args)
  (interactive
   (list
    (intern (completing-read "Theme: "
                             (mapcar #'symbol-name
				                     (custom-available-themes))))))
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (if (featurep (intern (format "%s-theme" theme)))
      (enable-theme theme)
    (apply f theme t no-enable args))
  (run-hooks 'after-load-theme-hook))
(advice-add 'load-theme :around #'my/load-theme)

(defun my/setup-theme ()
  (if (display-graphic-p)
      (load-theme my/theme t)
    (load-theme my/theme-tui t)))

(if (daemonp)
    (progn
      (add-hook 'server-after-make-frame-hook #'my/setup-font)
      (add-hook 'server-after-make-frame-hook #'my/setup-theme))
  (add-hook 'after-init-hook #'my/setup-font)
  (add-hook 'after-init-hook #'my/setup-theme))

(defun my/fixed-pitch-setup ()
  (interactive)
  (setq buffer-face-mode-face '(:family "Sarasa Mono SC"))
  (buffer-face-mode +1))

;;; Useful funcs

;; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
(defun memoize-remote (key cache orig-fn &rest args)
  "Memoize a value if the key is a remote path."
  (if (and key
           (file-remote-p key))
      (if-let* ((current (assoc key (symbol-value cache))))
          (cdr current)
        (let ((current (apply orig-fn args)))
          (set cache (cons (cons key current) (symbol-value cache)))
          current))
    (apply orig-fn args)))

(defun my/url-get-title (url &optional descr)
  "Takes a URL and returns the value of the <title> HTML tag.
   This function uses curl if available, and falls back to url-retrieve if not.
   It also handles UTF-8 encoded titles correctly."
  (when (or (string-prefix-p "http" url)
            (string-prefix-p "https" url))
    (let ((curl-available (executable-find "curl")))
      (with-temp-buffer
        (if curl-available
            (call-process "curl" nil t nil "-s" url)
          (let ((url-buf (url-retrieve-synchronously url)))
            (when url-buf
              (insert-buffer-substring url-buf)
              (kill-buffer url-buf))))
        (goto-char (point-min))
        (if (search-forward-regexp "<title>\\([^\n]+?\\)</title>" nil t)
            (decode-coding-string (match-string 1) 'utf-8)
          "No title found")))))

(defun retrieve-authinfo-key (host user)
  "从 .authinfo 中检索指定 HOST 和 USER 的密钥。"
  (interactive "sEnter host: \nsEnter user: ") ; 交互式输入 host 和 user
  ;; 使用 auth-source-search 来搜索匹配的条目
  (let ((credentials (auth-source-search :host host
                                         :user user
                                         :require '(:secret) ; 确保结果中包含密钥
                                         :max 1))) ; 最多返回一个结果
    (if credentials
        ;; 如果找到了凭据，使用 auth-source-secret 函数解析并返回密钥
        (let ((secret (funcall (plist-get (car credentials) :secret))))
          secret)
      ;; 如果没有找到凭据，显示消息
      (message "No credentials found for %s@%s." user host))))

(defun move-region-to-trash (start end)
  "Move the selected region to trash.el."
  (interactive "r")
  (let ((region-content (buffer-substring start end))
        (trash-file (expand-file-name "trash.el" user-emacs-directory)))
    ;; Ensure the file exists
    (unless (file-exists-p trash-file)
      (with-temp-buffer (write-file trash-file)))
    ;; Append the content to the trash file
    (with-temp-file trash-file
      (insert-file-contents trash-file)
      (goto-char (point-max))
      (insert "\n" region-content "\n"))
    ;; Optionally, delete the region from the original buffer
    (delete-region start end)))

;; https://www.emacswiki.org/emacs/BuildTags
;; Or generate manually, an expample for go file:
;; find . -type f -iname "*.go" | etags -
(defun create-etags (dir-name file-extension)
  "Create tags file in DIR-NAME for files matching FILE-EXTENSION."
  (interactive
   (list (read-directory-name "Directory: ")
         (read-regexp "Iname regexp (e.g., *.go): ")))
  (eshell-command
   (format "find %s -type f -iname \"%s\" | etags -" dir-name file-extension)))

(defun get-string-from-file (filePath)
  "Return file content as string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun format-second-timestamp (begin end)
  "Convert the selected region (a timestamp in seconds) to a formatted time string."
  (interactive "r")
  (let* ((timestamp-str (buffer-substring-no-properties begin end))
         (timestamp (string-to-number timestamp-str))
         (formatted-time (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time timestamp))))
    (message "%s" formatted-time)))

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
(keymap-substitute global-map #'move-beginning-of-line #'smarter-move-beginning-of-line)

(defun my/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha-background) 100))
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha-background newalpha))))))
(global-set-key (kbd "M-C-8") (lambda () (interactive) (my/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (my/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha-background . 100)))))

(defun my/delete-to-the-begining ()
  (interactive)
  (delete-region (point-min) (point)))

(defun my/delete-to-the-end ()
  (interactive)
  (delete-region (point) (point-max)))

(defun my/delete-whole-buffer ()
  (interactive)
  (delete-region (point-min) (point-max)))

(defun my/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-current-buffer)))


;;; Packages
;;;; C
(setq c-default-style "linux"
      c-basic-offset 4)

;;;; Compilation
;;
;; `compile'
(setq compilation-always-kill t
      compilation-ask-about-save nil
      compilation-scroll-output 'first-error)

;; Disable compiliation warnings
(setq warning-suppress-log-types '((comp)))


;;;; Comint
;;
;; `comint-mode' 是 Emacs 中用于实现交互式命令行缓冲区的基础模式（例如 shell、M-x term 等）
(setq comint-prompt-read-only t)
(setq comint-buffer-maximum-size 2048)

;;;; Dired
(setq mouse-drag-and-drop-region t
      mouse-drag-and-drop-region-cross-program t)

(setq dired-dwim-target t
      dired-vc-rename-file t
      dired-mouse-drag-files t
      dired-auto-revert-buffer t
      dired-recursive-copies 'always
      dired-create-destination-dirs 'ask
      dired-deletion-confirmer 'y-or-n-p
      dired-kill-when-opening-new-dired-buffer t
      dired-clean-confirm-killing-deleted-buffers nil)

(setq dired-omit-verbose nil)
(setq dired-omit-files (rx string-start
                           (or ".DS_Store"
                               ".cache"
                               ".vscode"
                               "__pycache__"
                               ".ccls-cache" ".clangd")
                           string-end))

(add-hook 'dired-mode-hook #'dired-omit-mode)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(with-eval-after-load 'dired
  (setq dired-listing-switches
        "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
  (keymap-set dired-mode-map "C-c C-p" #'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd "h") #'dired-up-directory)
  (define-key dired-mode-map [mouse-2] #'dired-find-file))

;;;; Ediff
;;
;; Configure Ediff to use a single frame and split windows horizontally
(setq ediff-window-setup-function #'ediff-setup-windows-plain
      ediff-split-window-function #'split-window-horizontally)


;;;; Eldoc
(setq eldoc-idle-delay 1
      eldoc-documentation-function 'eldoc-documentation-compose)

;; https://emacs-china.org/t/elisp/29204/7?u=rua
;; make eldoc display documentation too
(define-advice elisp-get-fnsym-args-string (:around (orig-fun sym &rest r) docstring)
  "If SYM is a function, append its docstring."
  (concat
   (apply orig-fun sym r)
   (let* ((doc (and (fboundp sym) (documentation sym 'raw)))
          (oneline (and doc (substring doc 0 (string-match "\n" doc)))))
     (and oneline
          (concat "  |  " (propertize oneline 'face 'italic))))))

;;;; Eglot
(setq eglot-autoshutdown t
      eglot-sync-connect nil ;; don't block of LSP connection attempts
      eglot-extend-to-xref t ;; make eglot manage file out of project by `xref-find-definitions'
      eglot-ignored-server-capabilites
      '(:documentHighlightProvider
        :documentFormattingProvider
        :documentRangeFormattingProvider
        :documentLinkProvider
        ;; 和 treesit 的缩进冲突
        :documentOnTypeFormattingProvider))

;; Eglot optimization
(setq jsonrpc-event-hook nil)
(setq eglot-events-buffer-size 0)
(setq eglot-report-progress nil)  ; Prevent Eglot minibuffer spam

;; Eglot optimization: Disable `eglot-events-buffer' to maintain consistent
;; performance in long-running Emacs sessions. By default, it retains 2,000,000
;; lines, and each new event triggers pretty-printing of the entire buffer,
;; leading to a gradual performance decline.
(setq eglot-events-buffer-config '(:size 0 :format full))


;; https://codeberg.org/mekeor/init/src/commit/11e3d86aa18090a5e3a6f0d29373c24373f29aaf/init.el#L813-L842
;; INFO: Translation:
;;   | JSON  | Eglot       |
;;   |-------+-------------|
;;   | true  | t           |
;;   | false | :json-false |
;;   | null  | nil         |
;;   | {}    | eglot-{}    |
(setq-default eglot-workspace-configuration
              '( :gopls ( :buildFlags ["-tags" "wireinject"]
                          :usePlaceholders t
                          :staticcheck t)
                 :pyright ( :checkOnlyOpenFiles t
                            :typeCheckingMode "basic")
                 :basedpyright ( :checkOnlyOpenFiles t
                                 :typeCheckingMode "basic")
                 ))

;; https://emacs-china.org/t/eglot-lsp-server-command/29566
(defun my/eglot-enable-command-provider (orig-fn server)
  "Unconditionally add :executeCommandProvider to Eglot client capabilities."
  (let ((original-capabilities (funcall orig-fn server)))
    ;; Add or update :executeCommandProvider at the top level
    (plist-put original-capabilities
               :executeCommandProvider '(:commands (:dynamicRegistration :json-false)))))
(advice-add 'eglot-client-capabilities :around #'my/eglot-enable-command-provider)

(defun my/eglot-execute-command (command)
  "Interactively execute a COMMAND supported by the current Eglot LSP server.
COMMAND is a string as advertised by the server. No arguments are passed."
  (interactive
   (let* ((server (eglot-current-server))
          (caps (eglot--capabilities server))
          (provider (plist-get caps :executeCommandProvider))
          (commands (and provider (plist-get provider :commands))))
     (list (completing-read "LSP Command: "
                            (or (cl-coerce commands 'list) '())
                            nil nil))))
  (eglot-execute (eglot-current-server) (list :command command)))

(with-eval-after-load 'eglot
  (keymap-set eglot-mode-map "M-RET" #'eglot-code-actions)
  (keymap-set eglot-mode-map "C-c r" #'eglot-rename)
  (keymap-set eglot-mode-map "M-'"   #'eglot-find-implementation)

  (add-to-list 'eglot-server-programs '(web-mode . ("svelteserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(sql-mode . ("sqls" "-config" "~/.config/sqls/config.yaml")))
  (add-to-list 'eglot-server-programs '(typst-ts-mode . ("typst-lsp")))
  (add-to-list 'eglot-server-programs '(org-mode . ("ltex-ls")))
  (add-to-list 'eglot-server-programs '(markdown-mode . ("ltex-ls")))
  (add-to-list 'eglot-server-programs '(message-mode . ("ltex-ls"))))

;;;; Flymake
(add-hook 'prog-mode-hook #'flymake-mode)
(add-hook 'emacs-lisp-mode-hook #'(lambda ()
                                    (flymake-mode -1)))

(setq-default flymake-diagnostic-functions nil)

;; https://www.reddit.com/r/emacs/comments/1ltp2j0/comment/n1u4rv1/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
(setq flymake-show-diagnostics-at-end-of-line 'fancy)

(defvar sekiro-flymake-mode-line-format `(:eval (sekiro-flymake-mode-line-format)))
(put 'sekiro-flymake-mode-line-format 'risky-local-variable t)
(defun sekiro-flymake-mode-line-format ()
  (let* ((counter (string-to-number
                   (nth 1
                        (cadr
                         (flymake--mode-line-counter :error)))))
         (sekiro-flymake (when (> counter 0)
                           'compilation-error)))
    (propertize
     "危"
     'face
     sekiro-flymake)))

(with-eval-after-load 'flymake
  (keymap-set flymake-mode-map "M-p" #'flymake-goto-prev-error)
  (keymap-set flymake-mode-map "M-n" #'flymake-goto-next-error)
  (add-to-list 'mode-line-misc-info
               `(flymake-mode (" [" sekiro-flymake-mode-line-format "] "))))

(add-hook 'flymake-mode-hook
          (lambda ()
            (add-hook 'eldoc-documentation-functions 'flymake-eldoc-function nil t)))

;;;; Help

;; Quick editing in `describe-variable'
(with-eval-after-load 'help-fns
  (put 'help-fns-edit-variable 'disabled nil))

;; If variable is a keymap, use `describe-keymap'.
(advice-add 'describe-variable :around
            (lambda (oldfun variable &optional buffer frame)
              (if (and (boundp variable)
                       (keymapp (symbol-value variable)))
                  (describe-keymap variable)
                (apply oldfun variable buffer frame))))

;;;; Hl-line
;;
;; Restrict `hl-line-mode' highlighting to the current window, reducing visual
;; clutter and slightly improving `hl-line-mode' performance.
(setq hl-line-sticky-flag nil)
(setq global-hl-line-sticky-flag nil)

(defun my/hl-line-setup ()
  "Disable `hl-line-mode' if region is active."
  (when (and (bound-and-true-p hl-line-mode)
             (region-active-p))
    (hl-line-unhighlight)))
(with-eval-after-load 'hl-line
  (add-hook 'post-command-hook #'my/hl-line-setup))
(add-hook 'prog-mode-hook #'global-hl-line-mode)

;;;; Hide show
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

(setq hs-set-up-overlay #'hideshow-folded-overlay-fn)

(add-hook 'prog-mode-hook #'hs-minor-mode)

;;;; Isearch
(setq isearch-lazy-count t
      isearch-lazy-highlight t
      lazy-highlight-buffer t
      ;; Don't be stingy with history; default is to keep just 16 entries
      search-ring-max 200
      regexp-search-ring-max 200
      ;; Record isearch in minibuffer history, so C-x ESC ESC can repeat it.
      isearch-resume-in-command-history t
      ;; M-< and M-> move to the first/last occurrence of the current search string.
      isearch-allow-motion t
      isearch-motion-changes-direction t
      ;; space matches any sequence of characters in a line.
      isearch-regexp-lax-whitespace t
      search-whitespace-regexp ".*?")

(keymap-global-set "C-s" #'isearch-forward-regexp)
(keymap-global-set "C-r" #'isearch-backward-regexp)

(with-eval-after-load "isearch"
  (define-advice isearch-occur (:after (_regexp &optional _nlines))
    "Exit isearch after calling."
    (isearch-exit))

  (keymap-set isearch-mode-map "C-c C-o" #'isearch-occur)
  ;; DEL during isearch should edit the search string, not jump back
  ;; to the previous result
  (keymap-substitute isearch-mode-map #'isearch-delete-chac #'isearch-del-chac))

;;;; Ispell
;;
;; In Emacs 30 and newer, disable Ispell completion to avoid annotation errors
;; when no `ispell' dictionary is set.
(setq text-mode-ispell-word-completion nil)

(setq ispell-silently-savep t)

;;;; Ibuffer
(fset 'list-buffers 'ibuffer)
(setq-default ibuffer-show-empty-filter-groups nil)
(setq ibuffer-formats
      '((mark modified read-only locked
              " " (name 40 40 :left :elide)
			  " " (size 8 -1 :right)
			  " " (mode 18 18 :left :elide) " " filename-and-process)
	    (mark " " (name 16 -1) " " filename)))

;;;; Icomplete
;;
;; Do not delay displaying completion candidates in `fido-mode' or
;; `fido-vertical-mode'
(setq icomplete-compute-delay 0.01)

;;;; GUD
(setq gud-highlight-current-line t)
(add-hook 'gud-mode-hook #'gud-tooltip-mode)

;;;; Outline
(setq outline-minor-mode-cycle t
      outline-minor-mode-highlight t)

;;;; Python
;;
;; Do not notify the user each time Python tries to guess the indentation offset
(setq python-indent-guess-indent-offset nil
      python-shell-dedicated 'project)

;; Two ways to make pyright work with installed package.
;;
;; 1. Use venv.
;; pyright need to know venvPath so that it can find the python packages
;; or raise error like "import can't be resolved"
;;
;; 2. Use pdm.
;; Packages installed with pdm under __pypackages__/<version>/lib/,
;; update pyproject.toml to make pyright work with it, for example:
;; [tool.pyright]
;; extraPaths = ["__pypackages__/3.8/lib/", "src/]
;; https://pdm-project.org/en/latest/usage/pep582/#emacs
;;
;; (also check basedpyright and delance)
(defun pyrightconfig-write ()
  "Write a `pyrightconfig.json' file at the root of a project with
`venvPath` and `venv`."
  (interactive)
  (let* ((json-encoding-pretty-print t)
         (fn (tramp-file-local-name python-shell-virtualenv-root))
         (venvPath (string-trim-right fn "/"))
         (out-file (expand-file-name "pyrightconfig.json" (project-root (project-current)))))
    (with-temp-file out-file
      (insert (json-encode (list :venvPath venvPath
                                 :venv ".venv"))))
    (message "Configured `%s` to use environment `%s`" out-file pyvenv-virtual-env)))

;;;; Project
(setq project-vc-ignores '("target/" "bin/" "obj/")
      project-vc-extra-root-markers '(".project"
                                      "go.mod"
                                      "Cargo.toml"
                                      "project.clj"
                                      "pyproject.toml"
                                      "pyrightconfig.json"
                                      "package.json"))

(with-eval-after-load 'project
  ;; Use fd in `project-find-file'
  (when (executable-find "fd")
    (defun my/project-files-in-directory (dir)
      "Use `fd' to list files in DIR."
      (let* ((default-directory dir)
             (localdir (file-local-name (expand-file-name dir)))
             (command (format "fd -c never -H -t f -0 . %s" localdir)))
        (project--remote-file-names
         (sort (split-string (shell-command-to-string command) "\0" t)
               #'string<))))
    (cl-defmethod project-files ((project (head local)) &optional dirs)
      "Override `project-files' to use `fd' in local projects."
      (mapcan #'my/project-files-in-directory
              (or dirs (list (project-root project)))))))

;; Memoize current project
(defvar project-current-cache nil)
(defun memoize-project-current (orig &optional prompt directory)
  (memoize-remote (or directory
                      project-current-directory-override
                      default-directory)
                  'project-current-cache orig prompt directory))
(with-eval-after-load 'project
  (advice-add 'project-current :around #'memoize-project-current))

;;;; Repeat
;;
;; Enable `repeat-mode' to reduce key sequence length
;; If we have been idle for `repeat-exit-timeout' seconds, exit the repeated
;; state.
(setq repeat-keep-prefix t
      repeat-exit-timeout 3
      repeat-exit-key (kbd "RET"))
(add-hook 'after-init-hook #'repeat-mode)

;;;; Simple.el
(add-hook 'prog-mode-hook
          #'(lambda ()
              (setq-local comment-auto-fill-only-comments t)
              (turn-on-auto-fill)))

;;;; Smerge
(add-hook 'find-file-hook #'(lambda ()
                              (save-excursion
                                (goto-char (point-min))
                                (when (re-search-forward "^<<<<<<< " nil t)
                                  (smerge-mode 1)))))

(with-eval-after-load 'smerge-mode
  (keymap-set smerge-mode-map "C-c c" #'smerge-keep-current)
  (keymap-set smerge-mode-map "C-c a" #'smerge-smerge-keep-all)
  (keymap-set smerge-mode-map "M-r" #'smerge-refine)
  (keymap-set smerge-mode-map "M-n" #'smerge-next)
  (keymap-set smerge-mode-map "M-p" #'smerge-prev))

;;;; Tramp
(setq tramp-verbose 0)

;; Set remote-file-name-inhibit-cache to nil if remote files are not
;; independently updated outside TRAMP’s control. That cache cleanup
;; will be necessary if the remote directories or files are updated
;; independent of TRAMP.
(setq remote-file-name-inhibit-cache nil)

;;  Disable file locks. Set remote-file-name-inhibit-locks to t if
;;  you know that different Emacs sessions are not modifying the same
;;  remote file.
(setq remote-file-name-inhibit-locks t)

;; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
(setq tramp-use-scp-direct-remote-copying t
      tramp-copy-size-limit 1000000
      remote-file-name-inhibit-auto-save-visited t)

(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)

(with-eval-after-load 'tramp
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook
                 #'tramp-compile-disable-ssh-controlmaster-options)))

(defun sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (when (file-writable-p file)
    (user-error "File is user writeable, aborting sudo"))
  (find-file (if (file-remote-p file)
		         (concat "/" (file-remote-p file 'method) ":"
			             (file-remote-p file 'user) "@" (file-remote-p file 'host)
			             "|sudo:root@"
			             (file-remote-p file 'host) ":" (file-remote-p file 'localname))
	           (concat "/sudo:root@localhost:" file))))

(defun sudo-this-file ()
  "Open the current file as root."
  (interactive)
  (sudo-find-file (file-truename buffer-file-name)))
(keymap-global-set "C-x C-z" #'sudo-this-file)

(with-eval-after-load 'tramp
  ;; ‘Private Directories’ are the settings of the $PATH environment,
  ;; as given in your ‘~/.profile’.  This entry is represented in
  ;; the list by the special value ‘tramp-own-remote-path’.
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;;;; Tab bar
;;
;; Built-in window layout manager
;; NOTE do not bind =tab-bar-switch-to-prev-tab= and
;; =tab-bar-switch-to-next-tab= to =M-[= or =M-]=, it will make emacs have some
;; bug to auto insert characters after you type everytime.
;;
;; See =tab-prefix-map= to custom key bindings for tab-bar, default is =C-x t=.
(defun tab-bar-format-menu-bar ()
  "Produce the Menu button for the tab bar that shows the menu bar."
  `((menu-bar menu-item
              (format " %s  "
                      (nerd-icons-sucicon "nf-custom-emacs"
                                          :face '(:inherit nerd-icons-purple)))
              tab-bar-menu-bar :help "Menu Bar")))

(defun my/bar-image ()
  (when (and (display-graphic-p) (image-type-available-p 'pbm))
    (propertize
     " " 'display
     (ignore-errors
       (create-image
        ;; 20 for `dirvish-header-line-height'
        (concat (format "P1\n%i %i\n" 2 30) (make-string (* 2 30) ?1) "\n")
        'pbm t :foreground (face-background 'highlight) :ascent 'center)))))

(setq tab-bar-new-tab-choice 'ibuffer
      tab-bar-close-last-tab-choice 'tab-bar-mode-disable
      tab-bar-tab-hints nil
      tab-bar-close-button-show nil
      tab-bar-separator ""
      tab-bar-format '(tab-bar-format-menu-bar
                       tab-bar-format-tabs)
      ;; NOTE 如果要用到很多 tab 导致 tab 换行的话就把这个设置为 t
      tab-bar-auto-width nil
      tab-bar-tab-name-format-function
      (lambda (tab i) "Center, taller, better, stronger xD."
        (let* ((current-tab-p (eq (car tab) 'current-tab))
               (bar (when current-tab-p (my/bar-image)))
               (name (string-trim (alist-get 'name tab)))
               (space-to-add (max 0 (- tab-bar-tab-name-truncated-max (length name))))
               (left-padding (/ space-to-add 2))
               (right-padding (- space-to-add left-padding)))
          (concat
           ;; bar
           (propertize (concat ;; (propertize " " 'display '(raise 0.3))
                        (make-string left-padding ?\s)
                        name
                        (make-string right-padding ?\s)
                        ;; (propertize " " 'display '(raise -0.3))
                        )
                       'face (funcall tab-bar-tab-face-function tab)))))
      tab-bar-tab-name-function
      (lambda nil "Use project as tab name."
        (let ((dir (expand-file-name
                    (or (if (and (fboundp 'project-root) (project-current))
                            (project-root (project-current)))
                        default-directory))))
          (or
           (and dir
                (let ((name (substring dir (1+ (string-match "/[^/]+/$" dir)) -1)))
                  (truncate-string-to-width name tab-bar-tab-name-truncated-max nil ? )))
           (buffer-name)))))

(with-eval-after-load 'tab-bar
  (tab-bar-history-mode 1))

;; https://www.emacs.dyerdwelling.family/emacs/20240817082349-emacs--syncing-tab-bar-to-theme/
(defun my/sync-tab-bar-to-theme ()
  "Synchronize tab-bar faces with the current theme."
  (interactive)
  (let ((default-bg (face-background 'default))
        (default-fg (face-foreground 'default))
        (inactive-fg (face-foreground 'mode-line-inactive)))
    (custom-set-faces
     `(tab-bar ((t (:inherit default :background ,default-bg :foreground ,default-fg))))
     `(tab-bar-tab ((t (:inherit default :background ,default-fg :foreground ,default-bg))))
     `(tab-bar-tab-inactive ((t (:inherit default :background ,default-bg :foreground ,inactive-fg)))))))
(add-hook 'after-load-theme-hook #'my/sync-tab-bar-to-theme)

;;;; vc
(setq-default vc-handled-backends '(Git))

;; memoize vc-git-root
(defvar vc-git-root-cache nil)
(defun memoize-vc-git-root (orig file)
  (let ((value (memoize-remote (file-name-directory file) 'vc-git-root-cache orig file)))
    ;; sometimes vc-git-root returns nil even when there is a root there
    (when (null (cdr (car vc-git-root-cache)))
      (setq vc-git-root-cache (cdr vc-git-root-cache)))
    value))
(with-eval-after-load 'vc
  (advice-add 'vc-git-root :around #'memoize-vc-git-root))

;;;; Which-key
;;
;; Allow C-h to trigger which-key before it is done automatically
(setq which-key-show-early-on-C-h t)
;; make sure which-key doesn't show normally but refreshes quickly after it is
;; triggered.
(setq which-key-idle-delay 10000)
(setq which-key-idle-secondary-delay 0.05)

(add-hook 'after-init-hook #'which-key-mode)

;;;; White spcae
;;
;; Show trailing whitespaces
;; https://list.orgmode.org/orgmode/Zqjm0hyy5DjFNrgm@swain.home.arpa/
(setq whitespace-style '(face trailing))
(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'conf-mode-hook #'whitespace-mode)

;;;; Xref
;;
;; Enable completion in the minibuffer instead of the definitions buffer
(setq xref-show-definitions-function #'xref-show-definitions-completing-read
      xref-show-xrefs-function #'xref-show-definitions-completing-read)

;; Fix massed xref cross multiple project.
(setq xref-history-storage 'xref-window-local-history)

(add-hook 'xref-after-return-hook #'recenter)
(add-hook 'xref-after-jump-hook #'recenter)
(keymap-global-unset "C-<down-mouse-1>")
(keymap-global-set "C-<mouse-1>" #'xref-find-definitions-at-mouse)

(with-eval-after-load 'xref
  (setq xref-search-program (cond ((executable-find "rg") 'ripgrep)
                                  (t 'grep))))

;;; Window
;;
;; The native border "uses" a pixel of the fringe on the rightmost
;; splits, whereas `window-divider-mode' does not.
(setq window-divider-default-bottom-width 1
      window-divider-default-places t
      window-divider-default-right-width 1)

;; Undo window change
(setq winner-dont-bind-my-keys t)
(add-hook 'after-init-hook #'winner-mode)

(keymap-global-set "M-o" 'other-window)

;; Skip window by setting no-other-window window parameter in
;; display-buffer-alist for specific buffer(like dired-sidebar).

;; When splitting window, show (other-buffer) in the new window
(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window with other-buffer unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (unless arg
        (set-window-buffer target-window (other-buffer))
        (select-window target-window)))))

(keymap-global-set "C-x 2" (split-window-func-with-other-buffer 'split-window-vertically))
(keymap-global-set "C-x 3" (split-window-func-with-other-buffer 'split-window-horizontally))

(defun sanityinc/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(keymap-global-set "C-x 1" 'sanityinc/toggle-delete-other-windows)

(defun split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(keymap-global-set "C-x |" 'split-window-horizontally-instead)
(keymap-global-set "C-x _" 'split-window-vertically-instead)

;;; init-builtin.el ends here
