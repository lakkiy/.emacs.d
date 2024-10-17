;;; init-builtin.el -*- lexical-binding: t; -*-

;;; var
(defvar my/fonts-default
  '("Monaco"
    "Cascadia Code"
    "Menlo"
    "Source Code Pro")
  "List of fonts to try when setting the default font.")

(defvar my/fonts-variable-pitch
  '("Bookerly"
    "Cardo"
    "Times New Roman"
    "DejaVu Sans")
  "List of fonts to try when setting the variable-pitch font.")

(defvar my/fonts-cjk
  '("LXGW WenKai"
    "FZYouSong GBK"
    "WenQuanYi Micro Hei"
    "Microsoft Yahei")
  "List of fonts to try when setting the CJK font.")

(defvar my/fonts-unicode '("Symbola")
  "List of fonts to try when setting the Unicode font.")

(defvar my/fonts-emoji
  '("Apple Color Emoji"
    "Segoe UI Symbol"
    "Noto Color Emoji")
  "List of fonts to try when setting the Emoji font.")

(defvar my/font-size-default 17
  "Default font size.")

(defvar my/theme 'modus-operandi
  "The default theme.")

(defvar my/theme-tui 'modus-vivendi
  "The default theme for TUI.")

(defvar after-load-theme-hook nil
  "Hooks run after `load-theme'.")

;;; disable stupid things

;; Reduce *Message* noise at startup. An empty scratch buffer (or the
;; dashboard) is more than enough, and faster to display.
(setq inhibit-startup-message t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t
      inhibit-x-resources t
      inhibit-default-init t
      server-client-instructions nil
      suggest-key-bindings nil)

;; Remove "For information about GNU Emacs..." message at startup
(fset #'display-startup-echo-area-message #'ignore)

;; Suppress GUI features and more
(setq use-file-dialog nil
      use-dialog-box nil)

;; No annoying bell
(setq ring-bell-function 'ignore)

;; No eyes distraction
(setq blink-cursor-mode nil)

;; Sane defaults
(setq use-short-answers t)

;; Inhibit switching out from `y-or-n-p' and `read-char-choice'
(setq y-or-n-p-use-read-key t
      read-char-choice-use-read-key t)

(keymap-global-unset "C-h h")
(keymap-global-unset "M-z")
(when (display-graphic-p)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z")))

;;; better default
(setq initial-buffer-choice nil
      initial-major-mode 'fundamental-mode)

;; Pixelwise resize
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

;; Dont move points out of eyes
(setq mouse-yank-at-point t)

(setq-default fill-column 80)

;; Linux specific
(setq x-gtk-use-system-tooltips nil
      x-gtk-use-native-input t
      x-gtk-resize-child-frames 'resize-mode
      x-underline-at-descent-line t)

;; Enable the disabled narrow commands
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)
;; mouse-1 on a button should follow the link
(put 'default-button 'follow-link t)

;; Enable the disabled dired commands
(put 'dired-find-alternate-file 'disabled nil)

;; No Fcitx5 in Emacs PGTK build.
(setq pgtk-use-im-context-on-new-connection nil)

(setq outline-minor-mode-cycle t
      outline-minor-mode-highlight t)

(setq completion-styles '(basic partial-completion)
      completion-category-overrides '((file (styles basic partial-completion))))

;; Buffer manager
(fset 'list-buffers 'ibuffer)
(setq-default ibuffer-show-empty-filter-groups nil)

;; Better word wrapping for CJK characters
(setq word-wrap-by-category t)
(setq sentence-end-double-space nil)

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;;; modern editor config

;; Contrary to what many Emacs users have in their configs, you don't need
;; more than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")
;; Set-language-environment sets default-input-method, which is unwanted.
(setq default-input-method nil)

;; No tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Tab to complete
(setq tab-always-indent 'complete)

;; Back to the previous position
(add-hook 'after-init-hook #'save-place-mode)

;; Needed by `webpaste'
(setq browse-url-generic-program
      (or (when (eq system-type 'darwin) "open")
          (when (eq system-type 'gnu/linux) "xdg-open")))

;; Save minibuffer history
(setq history-length 1000)
(add-hook 'after-init-hook #'savehist-mode)

;; Echo current unfinished command immediately.
(setq echo-keystrokes 0.1)

(add-hook 'after-init-hook #'context-menu-mode)
(add-hook 'after-init-hook #'global-auto-revert-mode)
(add-hook 'after-init-hook #'global-goto-address-mode)
(add-hook 'after-save-hook #'delete-trailing-whitespace)

(setq require-final-newline t
      visible-cursor t
      vc-follow-symlinks t)

(setq display-line-numbers-width 3)

;;; performance

;; Improve display
(setq display-raw-bytes-as-hex t
      redisplay-skip-fontification-on-input t)

;; Increase how much is read from processes in a single chunk (default is 4kb).
(setq read-process-output-max (* 4 1024 1024)
      process-adaptive-read-buffering nil)

;; Reduce rendering/line scan work by not rendering cursors or regions in
;; non-focused windows.
(setq-default cursor-in-non-selected-windows nil)

;; Disable warnings from the legacy advice API. They aren't useful.
(setq ad-redefinition-action 'accept)

;; Disable compiliation warnings
(setq warning-suppress-log-types '((comp)))

;; Ignore warnings about "existing variables being aliased".
(setq warning-suppress-types '((defvaralias) (lexical-binding)))

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
;; No second pass of case-insensitive search over auto-mode-alist.
(setq auto-mode-case-fold nil)

;; Optimize for long line
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

;;; autosave, backup, lockfiles

;; No backup files
(setq make-backup-files nil
      auto-save-default nil)

;; No lock files
(setq create-lockfiles nil)

;; Most of time I will do save manually.
(setq auto-save-visited-interval 10)
(add-hook 'after-init-hook #'auto-save-visited-mode)

;;; scrolling

(setq scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)

;; The nano style for truncated long lines.
(setq auto-hscroll-mode 'current-line)

;; Disable auto vertical scroll for tall lines
(setq auto-window-vscroll nil)

;; Use shift + mouse wheel to scrll horizontally.
(setq mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

;; smooth scroll up & down
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

;;; path
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

;;; font & theme
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

(setq modus-themes-fringes nil)

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

;;; useful funcs
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

;;; electric-pair
(add-hook 'prog-mode-hook #'electric-pair-local-mode)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)

;;; help

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

;;; repeat
;;
;; Enable `repeat-mode' to reduce key sequence length
;; If we have been idle for `repeat-exit-timeout' seconds, exit the repeated
;; state.
(setq repeat-keep-prefix t
      repeat-exit-timeout 3
      repeat-exit-key (kbd "RET"))
(add-hook 'after-init-hook #'repeat-mode)

;;; recentf
(keymap-global-set "C-x C-r" #'recentf-open-files)
(add-hook 'after-init-hook #'recentf-mode)
(setq recentf-max-saved-items 1000
      recentf-auto-cleanup 'never
      recentf-exclude `(,tramp-file-name-regexp
                        "COMMIT_EDITMSG"))

;;; paren
;;
;; Highlight parenthesises
(setq show-paren-when-point-in-periphery t
      show-paren-context-when-offscreen 'overlay
      show-paren-when-point-inside-paren t
      show-paren-context-when-offscreen t)
(add-hook 'after-init-hook #'show-paren-mode)

;;; tramp
(setq-default vc-handled-backends '(Git))

(setq tramp-verbose 0
      tramp-default-method "ssh")

;; Set remote-file-name-inhibit-cache to nil if remote files are not
;; independently updated outside TRAMP’s control. That cache cleanup
;; will be necessary if the remote directories or files are updated
;; independent of TRAMP.
(setq remote-file-name-inhibit-cache nil)

;;  Disable file locks. Set remote-file-name-inhibit-locks to t if
;;  you know that different Emacs sessions are not modifying the same
;;  remote file.
(setq remote-file-name-inhibit-locks t)

;; Speed up complete
(setq tramp-completion-reread-directory-timeout nil
      tramp-auto-save-directory temporary-file-directory)

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

;;; project
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

;;; dired
(setq mouse-drag-and-drop-region t
      mouse-drag-and-drop-region-cross-program t)

(setq dired-mouse-drag-files t
      dired-dwim-target t
      dired-kill-when-opening-new-dired-buffer t
      dired-auto-revert-buffer t
      delete-by-moving-to-trash t)

(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(with-eval-after-load 'dired
  (setq dired-listing-switches
        "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
  (keymap-set dired-mode-map "C-c C-p" #'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd "h") #'dired-up-directory)
  (define-key dired-mode-map [mouse-2] #'dired-find-file))

;;; isearch
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

;;; which-key
;;
;; Allow C-h to trigger which-key before it is done automatically
(setq which-key-show-early-on-C-h t)
;; make sure which-key doesn't show normally but refreshes quickly after it is
;; triggered.
(setq which-key-idle-delay 10000)
(setq which-key-idle-secondary-delay 0.05)

(add-hook 'after-init-hook #'which-key-mode)

;;; window

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

;;; tab-bar
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

;;; package.el
(setq package-archives
      '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
	    ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
;; To prevent initializing twice
(setq package-enable-at-startup nil)
;; gptel need newest org mode
(setq package-install-upgrade-built-in t)
(setq package-quickstart t)
(package-initialize)

(defun install-package (pkg &optional url)
  (unless (package-installed-p pkg)
    (if url
        (package-vc-install url)
      (unless (package-installed-p pkg)
        (package-refresh-contents))
      (package-install pkg))))


;;; init-builtin.el ends here
