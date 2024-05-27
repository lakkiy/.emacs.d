;;; init-must.el -*- lexical-binding: t; -*-

;; Contrary to what many Emacs users have in their configs, you don't need
;; more than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")

;; Speed up process data
(setq read-process-output-max (* 4 1024 1024)
      process-adaptive-read-buffering nil)

;; Suppress GUI features and more
(setq use-file-dialog nil
      use-dialog-box nil
      suggest-key-bindings nil
      initial-major-mode 'fundamental-mode
      inhibit-x-resources t
      inhibit-default-init t
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-buffer-menu t
      warning-suppress-log-types '((comp))
      server-client-instructions nil)
(fset #'display-startup-echo-area-message #'ignore)

;; Pixelwise resize
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

;; Linux specific
(setq x-gtk-use-system-tooltips nil
      x-gtk-use-native-input t
      x-gtk-resize-child-frames 'resize-mode
      x-underline-at-descent-line t)

;; With GPG 2.1+, this forces gpg-agent to use the Emacs minibuffer to prompt
;; for the key passphrase.
(setq epg-pinentry-mode 'loopback)

;; Optimize for long line
;; https://emacs-china.org/t/topic/25811/9?u=rua
(setq-default bidi-display-reordering 'left-to-right)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)
(add-hook 'after-init-hook #'global-so-long-mode)

;; No backup files
(setq make-backup-files nil
      auto-save-default nil)

;; No lock files
(setq create-lockfiles nil)

;; Always load the newest file
(setq load-prefer-newer t)

;; No gc for font caches
(setq inhibit-compacting-font-caches t)

;; Improve display
(setq display-raw-bytes-as-hex t
      redisplay-skip-fontification-on-input t)

;; No annoying bell
(setq ring-bell-function 'ignore)

;; No eyes distraction
(setq blink-cursor-mode nil)

;; Smooth scroll & friends
(setq scroll-step 2
      scroll-margin 2
      hscroll-step 2
      hscroll-margin 2
      scroll-conservatively 101
      scroll-preserve-screen-position 'always)

;; The nano style for truncated long lines.
(setq auto-hscroll-mode 'current-line)

;; Disable auto vertical scroll for tall lines
(setq auto-window-vscroll nil)

;; Use shift + mouse wheel to scrll horizontally.
(setq mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

;; Smooth scroll up & down
(setq pixel-scroll-precision-interpolate-page t)

(defun +pixel-scroll-interpolate-down (&optional lines)
  (interactive)
  (if lines
      (pixel-scroll-precision-interpolate (* -1 lines (pixel-line-height)))
    (pixel-scroll-interpolate-down)))

(defun +pixel-scroll-interpolate-up (&optional lines)
  (interactive)
  (if lines
      (pixel-scroll-precision-interpolate (* lines (pixel-line-height))))
  (pixel-scroll-interpolate-up))

(defalias 'scroll-up-command '+pixel-scroll-interpolate-down)
(defalias 'scroll-down-command '+pixel-scroll-interpolate-up)

;; Dont move points out of eyes
(setq mouse-yank-at-point t)

(setq-default fill-column 80)

;; No tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Tab to complete
(setq tab-always-indent 'complete)

;; Sane defaults
(setq use-short-answers t)

;; Inhibit switching out from `y-or-n-p' and `read-char-choice'
(setq y-or-n-p-use-read-key t
      read-char-choice-use-read-key t)

;; Enable the disabled narrow commands
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)
;; mouse-1 on a button should follow the link
(put 'default-button 'follow-link t)

;; Enable the disabled dired commands
(put 'dired-find-alternate-file 'disabled nil)

;; Enable the disabled `list-timers', `list-threads' commands
(put 'list-timers 'disabled nil)
(put 'list-threads 'disabled nil)

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

;; Most of time I will do save manually.
(setq auto-save-visited-interval 10)
(add-hook 'after-init-hook #'auto-save-visited-mode)

;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; No Fcitx5 in Emacs PGTK build.
(setq pgtk-use-im-context-on-new-connection nil)

;; Highlight parenthesises
(setq show-paren-when-point-in-periphery t
      show-paren-context-when-offscreen 'overlay
      show-paren-when-point-inside-paren t
      show-paren-context-when-offscreen t)
(add-hook 'after-init-hook #'show-paren-mode)

;; Undo window change
(setq winner-dont-bind-my-keys t)
(add-hook 'after-init-hook #'winner-mode)

;; Back to the previous position
(add-hook 'after-init-hook #'save-place-mode)

;; Needed by `webpaste'
(setq browse-url-generic-program
      (or (executable-find "firefox")
          (when (eq system-type 'darwin) "open")
          (when (eq system-type 'gnu/linux) "xdg-open")))

;; Buffer manager
(fset 'list-buffers 'ibuffer)
(setq-default ibuffer-show-empty-filter-groups nil)

;; Enable `repeat-mode' to reduce key sequence length
;;
;; If we have been idle for `repeat-exit-timeout' seconds, exit the repeated
;; state.
(setq repeat-mode t
      repeat-keep-prefix t
      repeat-exit-timeout 3
      repeat-exit-key (kbd "RET"))

;; Better word wrapping for CJK characters
(setq word-wrap-by-category t)
(setq sentence-end-double-space nil)

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Save minibuffer history
(setq history-length 1000)
(add-hook 'after-init-hook #'savehist-mode)

;; Recently opened files
(keymap-global-set "C-x C-r" #'recentf-open-files)
(add-hook 'after-init-hook #'recentf-mode)
(setq recentf-max-saved-items 1000
      recentf-auto-cleanup 'never
      recentf-exclude `(,tramp-file-name-regexp
                        "COMMIT_EDITMSG"))

;; Echo current unfinished command immediately.
(setq echo-keystrokes 0.1)

(defun my/init-func ()
  (context-menu-mode 1)
  (global-auto-revert-mode 1)
  (global-goto-address-mode 1)
  (pixel-scroll-precision-mode 1))
(add-hook 'after-init-hook #'my/init-func)



(setq outline-minor-mode-cycle t
      outline-minor-mode-highlight t)

(setq completion-styles '(basic partial-completion)
      completion-category-overrides '((file (styles basic partial-completion))))

(setq require-final-newline t
      cursor-in-non-selected-windows nil
      visible-cursor t
      vc-follow-symlinks t)

(setq display-line-numbers-width 3)

;; electric-pair
(add-hook 'prog-mode-hook #'electric-pair-local-mode)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)

(keymap-global-unset "C-h h")
(keymap-global-unset "M-z")
(when (display-graphic-p)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z")))


;; tramp
(setq-default vc-handled-backends '(Git))

(setq tramp-verbose 0
      tramp-terminal-type "tramp"
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


;; project
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


;; dired
(setq mouse-drag-and-drop-region t
      mouse-drag-and-drop-region-cross-program t)

(setq dired-mouse-drag-files t
      dired-dwim-target t
      dired-kill-when-opening-new-dired-buffer t
      dired-auto-revert-buffer t)

(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(with-eval-after-load 'dired
  (setq dired-listing-switches
        "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
  (keymap-set dired-mode-map "C-c C-p" #'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd "h") #'dired-up-directory)
  (define-key dired-mode-map [mouse-2] #'dired-find-file))


;; isearch
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


;;; init-must.el ends here
(provide 'init-must)
