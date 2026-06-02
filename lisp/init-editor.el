;;; init-editor.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

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

;; 禁用自动垂直滚动，以解决编辑超长行时的光标滞后和屏幕跳动问题
(setq auto-window-vscroll nil)

;; Horizontal scrolling, use shift + mouse wheel to scrll horizontally.
(setq hscroll-margin 2
      hscroll-step 1
      mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2
      ;; Wheel acceleration makes each notch's scroll distance vary, so the
      ;; interpolated animations end up uneven and stutter -- keep it constant.
      mouse-wheel-progressive-speed nil)

;; Smooth scroll up & down
(setq pixel-scroll-precision-interpolate-page t)
;; A trackpad emits pixel-precise deltas, but a mouse wheel emits discrete
;; line-granularity events that aren't interpolated by default (the threshold
;; below is nil out of the box), so the wheel scrolls in jumps.  Setting a
;; pixel threshold makes any scroll taller than it animate too -- this is what
;; smooths the mouse wheel.
(setq pixel-scroll-precision-large-scroll-height 40.0)
;; Shorter per-notch animation completes faster, so rapid wheel notches don't
;; queue up and lag behind.
(setq pixel-scroll-precision-interpolation-total-time 0.07)
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
  (keymap-substitute isearch-mode-map #'isearch-delete-char #'isearch-del-char))

;;;; Ibuffer
(fset 'list-buffers 'ibuffer)
(setq-default ibuffer-show-empty-filter-groups nil)
(setq ibuffer-formats
      '((mark modified read-only locked
              " " (name 40 40 :left :elide)
			  " " (size 8 -1 :right)
			  " " (mode 18 18 :left :elide) " " filename-and-process)
	    (mark " " (name 16 -1) " " filename)))

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

;;; init-editor.el ends here
