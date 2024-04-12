;;; init-must.el --- Must have config -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

;;; perf
(setq redisplay-skip-fontification-on-input t
      frame-resize-pixelwise t
      ;; NOTE this may cause lsp-bridge-ref buffer didn't show.
      window-resize-pixelwise nil
      ;; Don't pass case-insensitive to `auto-mode-alist'
      auto-mode-case-fold nil
      ;; Don't ping things that look like domain names.
      ffap-machine-p-known 'reject)

;; For lsp
(setq read-process-output-max (* 4 1024 1024)
      process-adaptive-read-buffering nil)

;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)

;; Long line
(add-hook 'after-init-hook #'global-so-long-mode)

;; https://emacs-china.org/t/topic/25811/9?u=rua
(setq-default bidi-display-reordering 'left-to-right)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

;;; inhibit startup
(setq inhibit-startup-screen t
      ;; Don't load default.el, only init.el
      inhibit-default-init t
      ;; Don't use prog-mode an stratup
      initial-major-mode 'fundamental-mode)
;; Shut up!
(defun display-startup-echo-area-message() nil)

;;; Disable some annoying UI features
(setq ring-bell-function 'ignore
      x-underline-at-descent-line t
      ;; Disable gui box, use minibuffer to comfirm action.
      use-file-dialog nil
      use-dialog-box nil
      ;; yes or no -> y or n
      use-short-answers t
      ;; Don't use Fcitx5 in Emacs in PGTK build.
      pgtk-use-im-context-on-new-connection nil
      ;; Avoid breakage of childframes.
      x-gtk-resize-child-frames 'resize-mode
      ;; Don't use GTK+ tooltip.
      x-gtk-use-system-tooltips nil
      ;; Disable "You can run the command balabala..."
      suggest-key-bindings nil)
(add-hook 'after-init-hook (lambda () (blink-cursor-mode -1)))

;;; TODO
(add-hook 'after-init-hook #'global-goto-address-mode)
(add-hook 'after-init-hook #'context-menu-mode)

(setq outline-minor-mode-cycle t
      outline-minor-mode-highlight t)

(add-hook 'after-init-hook #'winner-mode)
(setq winner-dont-bind-my-keys t)

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
(setq read-extended-command-predicate #'command-completion-default-include-p)
(setq completion-styles '(basic partial-completion)
      completion-category-overrides '((file (styles basic partial-completion))))
(setq require-final-newline t
      ;; Echo current unfinished command immediately.
      echo-keystrokes 0.1
      ;; 默认软折行是根据空格来的，但是中文句子没有空格，所以需要开启
      word-wrap-by-category t
      cursor-in-non-selected-windows nil
      visible-cursor t
      warning-suppress-log-types '((comp)) ; Don't display compile warnings
      truncate-partial-width-windows 65 ; Don't truncate lines in a window narrower than 65 chars.
      vc-follow-symlinks t)
;; Monitors are trending toward wide, rather than tall.
(setq split-width-threshold 160)
(setq split-height-threshold nil)

;;; Disable lockfiles and backup files
(setq create-lockfiles nil
      make-backup-files nil
      auto-save-default nil)

;;; auto-fill
(setq-default fill-column 72)
(add-hook 'prog-mode-hook
          #'(lambda ()
              (setq-local comment-auto-fill-only-comments t)
              (turn-on-auto-fill)))

;;; line number
(setq display-line-numbers-width 3)

;;; Enable auto save.
;;
;; Most of time I will do save manually.
(setq auto-save-visited-interval 10)
(add-hook 'after-init-hook #'auto-save-visited-mode)

;;; Tab
;;
;; Indent with spaces, not tabs, also use tab do complete.
(setq-default indent-tabs-mode nil)
(setq tab-width 4
      tab-always-indent 'complete)

;;; subword
(add-hook 'prog-mode-hook #'subword-mode)

;;; electric-pair
(add-hook 'prog-mode-hook #'electric-pair-local-mode)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)

;;; show-paren
(setq show-paren-when-point-in-periphery t
      show-paren-context-when-offscreen 'overlay
      show-paren-when-point-inside-paren t
      show-paren-context-when-offscreen t)

;;; Coding system
;;
;; Contrary to what many Emacs users have in their configs, you don't need
;; more than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")

;;; Delete whitespace after save
(setq whitespace-style '(face trailing))
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'conf-mode-hook #'whitespace-mode)

;;; Auto revert edited file.
(add-hook 'after-init-hook #'global-auto-revert-mode)

;;; scroll
(setq hscroll-step 1
      hscroll-margin 2
      ;; The nano style for truncated long lines.
      auto-hscroll-mode 'current-line
      scroll-margin 0
      scroll-conservatively 101
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      ;; Use shift + mouse wheel to scrll horizontally.
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2
      pixel-scroll-precision-interpolate-page t)

(add-hook 'after-init-hook (lambda () (pixel-scroll-precision-mode)))

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

;;; help
;;
;; Press e to edit variable value in help buffer.
(setq help-enable-variable-value-editing t)
(put 'help-fns-edit-variable 'disabled nil)

;; 会卡住 emacs，因为要显示多国字体
(keymap-global-unset "C-h h")
;; zap-to-char 经常误触，而且会常驻在 minibuffer，点过去取消后还会打乱
;; window layout
(keymap-global-unset "M-z")

(when (display-graphic-p)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z")))

;; If variable is a keymap, use `describe-keymap'.
(advice-add 'describe-variable :around
            (lambda (oldfun variable &optional buffer frame)
              (if (and (boundp variable)
                       (keymapp (symbol-value variable)))
                  (describe-keymap variable)
                (apply oldfun variable buffer frame))))

;;; vc
;;
;; Also speed up version control on tramp.
(setq-default vc-handled-backends '(Git))

;;; tramp
(setq tramp-terminal-type "tramp"
      ;; Set remote-file-name-inhibit-cache to nil if remote files are not
      ;; independently updated outside TRAMP’s control. That cache cleanup
      ;; will be necessary if the remote directories or files are updated
      ;; independent of TRAMP.
      remote-file-name-inhibit-cache nil
      ;;  Disable file locks. Set remote-file-name-inhibit-locks to t if
      ;;  you know that different Emacs sessions are not modifying the same
      ;;  remote file.
      remote-file-name-inhibit-locks t
      ;; Disable excessive traces.
      tramp-verbose 0
      ;; C-x C-f /ssh:
      tramp-default-method "ssh"
      ;;  speed up complete
      tramp-completion-reread-directory-timeout nil
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

;;; repeat command
;;
;; Use `repeat' to rerun previout command.
(setq repeat-mode t
      repeat-keep-prefix t
      repeat-exit-timeout 3
      repeat-exit-key (kbd "RET"))

;;; recentf
(add-hook 'after-init-hook #'recentf-mode)
(setq recentf-max-saved-items 1000
      recentf-exclude `(,tramp-file-name-regexp
                        "COMMIT_EDITMSG"))
(keymap-global-set "C-x C-r" #'recentf-open-files)

;;; project
(with-eval-after-load 'project
  ;; use fd in `project-find-file'
  (defun eat/project-files-in-directory (dir)
    "Use `fd' to list files in DIR."
    (let* ((default-directory dir)
           (localdir (file-local-name (expand-file-name dir)))
           (command (format "fd -c never -H -t f -0 . %s" localdir)))
      (project--remote-file-names
       (sort (split-string (shell-command-to-string command) "\0" t)
             #'string<))))
  (when (executable-find "fd")
    (cl-defmethod project-files ((project (head local)) &optional dirs)
      "Override `project-files' to use `fd' in local projects."
      (mapcan #'eat/project-files-in-directory
              (or dirs (list (project-root project))))))
  (setq project-vc-ignores '("target/" "bin/" "obj/")
        project-vc-extra-root-markers '(".project"
                                        "go.mod"
                                        "Cargo.toml"
                                        "project.clj"
                                        "pom.xml"
                                        "package.json"
                                        "Makefile"
                                        "README.org"
                                        "README.md")))

;;; savehist and place
(setq history-length 1000)
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'save-place-mode)

;;; dired
(setq mouse-drag-and-drop-region t
      mouse-drag-and-drop-region-cross-program t)

(setq dired-mouse-drag-files t
      dired-dwim-target t
      dired-kill-when-opening-new-dired-buffer t
      dired-auto-revert-buffer t)

(with-eval-after-load 'dired
  (setq dired-listing-switches
        "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
  (keymap-set dired-mode-map "C-c C-p" #'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd "h") #'dired-up-directory)
  (define-key dired-mode-map [mouse-2] #'dired-find-file))

;;; ibuffer
(fset 'list-buffers 'ibuffer)
(setq-default ibuffer-show-empty-filter-groups nil)

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

;;; browse-url
(setq browse-url-generic-program
      (or (executable-find "firefox")
          (when (eq system-type 'darwin) "open")
          (when (eq system-type 'gnu/linux) "xdg-open")))

;;; theme
(defvar my/theme 'paperlike
  "Default theme.")

(defvar after-load-theme-hook nil
  "Hooks run after `load-theme'.")

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

(add-hook 'after-init-hook #'(lambda ()
                               (when (display-graphic-p)
                                 (load-theme my/theme))))

;;; init-must.el ends here
(provide 'init-must)
