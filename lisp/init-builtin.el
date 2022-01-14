;;; -*- lexical-binding: t -*-

;; `recentf'
;; (add-hook 'after-init-hook #'recentf-mode)
(global-set-key (kbd "C-x C-r") #'recentf-open-files)

(setq recentf-max-saved-items 300
      recentf-exclude
      '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
        "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
        "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
        "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
        (lambda (file) (file-in-directory-p file package-user-dir))))

(with-eval-after-load 'recentf
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))

;; `display-line-numbers'
;; (add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; `subword'
(add-hook 'prog-mode-hook #'subword-mode)

;; `simple'
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; `so-long'
(add-hook 'after-init-hook #'global-so-long-mode)

;; `repeat'
(setq
 repeat-mode t
 repeat-keep-prefix t
 repeat-exit-timeout 3
 repeat-exit-key (kbd "RET"))

;; `hl-line'
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'conf-mode-hook #'hl-line-mode)

;; `autorevert'
(add-hook 'after-init-hook #'global-auto-revert-mode)

;; `elec-pair'
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(add-hook 'prog-mode-hook #'electric-pair-mode)

;; `saveplace'
(add-hook 'after-init-hook #'save-place-mode)

;; `paren'
(setq
 show-paren-when-point-in-periphery t
 show-paren-when-point-inside-paren t)

(add-hook 'prog-mode-hook #'show-paren-mode)

;; `tramp'
(setq
 ;; Always use file cache when using tramp
 remote-file-name-inhibit-cache nil
 ;; C-x C-f /ssh:
 tramp-default-method "ssh")

;; `eldoc'
(setq eldoc-idle-delay 2)

;; `whitespace'
(setq whitespace-style '(face trailing))

(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'conf-mode-hook #'whitespace-mode)

;; `hideshow'
(setq +hs-folding-fringe-indicators t)

(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap '+hs-folding-fringe-marker
    (vector #b00000000
            #b00000000
            #b00000000
            #b11000011
            #b11100111
            #b01111110
            #b00111100
            #b00011000)))

(defface +hs-folding-fringe-face
  '((t (:inherit 'font-lock-comment-face
                 :box (:line-width 1 :style released-button))))
  "Face for folding bitmaps appearing on the fringe.")

(defface +hs-folding-face
  '((t (:inherit 'font-lock-comment-face :box t)))
  "Face for the folded region indicator.")

(defun +hs-display-code-line-counts (ov)
  "Display a folded region indicator with the number of folded
      lines.

    Meant to be used as `hs-set-up-overlay'."
  (let* ((marker-string "*fringe-dummy*")
         (marker-length (length marker-string)))
    (cond
     ((eq 'code (overlay-get ov 'hs))
      (let* ((nmb-line (count-lines (overlay-start ov)
                                    (overlay-end ov)))
             (display-string (format "(%d)..." nmb-line)))
        ;; fringe indicator
        (when +hs-folding-fringe-indicators
          (put-text-property 0 marker-length 'display
                             (list 'left-fringe
                                   '+hs-folding-fringe-marker
                                   '+hs-folding-fringe-face)
                             marker-string)
          (overlay-put ov 'before-string marker-string)
          (overlay-put ov '+hs-fringe t))
        ;; folding indicator
        (put-text-property 0 (length display-string)
                           'face '+hs-folding-face
                           display-string)
        (put-text-property 0 (length display-string)
                           'mouse-face 'highlight display-string)
        (overlay-put ov 'display display-string)
        (overlay-put ov '+hs-folded t)))
     ;; for docstring and comments, we don't display the number of
     line
     ((or (eq 'docstring (overlay-get ov 'hs))
          (eq 'comment (overlay-get ov 'hs)))
      (let ((display-string "..."))
        (put-text-property 0 (length display-string)
                           'mouse-face 'highlight display-string)
        (overlay-put ov 'display display-string)
        (overlay-put ov '+hs-folded t))))))

(setq hs-set-up-overlay #'+hs-display-code-line-counts)

(add-hook 'prog-mode-hook #'hs-minor-mode)

;; `xref'
(global-unset-key (kbd "C-<down-mouse-1>"))
(global-set-key (kbd "C-<mouse-1>") #'xref-find-definitions-at-mouse)

(setq
 xref-prompt-for-identifier nil
 xref-search-program 'ripgrep
 xref-show-xrefs-function #'xref-show-definitions-completing-read
 xref-show-definitions-function #'xref-show-definitions-completing-read)

;; `winner'
(setq winner-dont-bind-my-keys t)

(add-hook 'after-init-hook #'winner-mode)

;; `smerge-mode'
(add-hook 'find-file-hook
          #'(lambda ()
              (save-excursion
                (goto-char (point-min))
                (when (re-search-forward "^<<<<<<< " nil t)
                  (smerge-mode 1)))))

(with-eval-after-load 'smerge-mode
  (define-key smerge-mode-map (kbd "M-r") #'smerge-refine)
  (define-key smerge-mode-map (kbd "M-RET") #'smerge-keep-current))

;; `dired'
(setq dired-dwim-target t)

;; `ibuffer'
(fset 'list-buffers 'ibuffer)

;; `ediff'
(setq
 ediff-window-setup-function #'ediff-setup-windows-plain
 ediff-highlight-all-diffs t
 ediff-split-window-function 'split-window-horizontally
 ediff-merge-split-window-function 'split-window-horizontally)

;; `flyspell' -- only enable in magit commit
(setq flyspell-issue-welcome-flag nil
      flyspell-issue-message-flag nil
      ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))

(add-hook 'flyspell-mode-hook
          #'(lambda ()
              (dolist (key '("C-;" "C-," "C-." "C-M-i"))
                (define-key flyspell-mode-map (kbd key) nil))))

;; `project'
(defun my/project-files-in-directory (dir)
  "Use `fd' to list files in DIR."
  (let* ((default-directory dir)
         (localdir (file-local-name (expand-file-name dir)))
         (command (format "fd -H -t f -0 . %s" localdir)))
    (project--remote-file-names
     (sort (split-string (shell-command-to-string command) "\0" t)
           #'string<))))

(when (executable-find "fd")
  (cl-defmethod project-files ((project (head local)) &optional dirs)
    "Override `project-files' to use `fd' in local projects."
    (mapcan #'my/project-files-in-director
            (or dirs (list (project-root project))))))

(defun +project-name ()
  (file-name-nondirectory (directory-file-name (project-root (project-current)))))

;; `tab-bar'
(setq
 tab-bar-border nil
 tab-bar-close-button nil
 tab-bar-back-button nil
 tab-bar-new-button nil
 tab-bar-format '(tab-bar-format-tabs)
 tab-bar-tab-name-format-function '+tab-bar-tab-format-function
 tab-bar-tab-name-truncated-max 10)

(defun +tab-bar-switch-project ()
  "Switch to project in a new tab, project name will be used as tab name.

No tab will created if the command is cancelled."
  (interactive)
  (let (succ)
    (unwind-protect
        (progn
          (tab-bar-new-tab)
          (call-interactively #'project-switch-project)
          (when-let ((proj (project-current)))
            (tab-bar-rename-tab (format "%s" (file-name-nondirectory (directory-file-name (cdr proj)))))
            (setq succ t)))
      (unless succ
        (tab-bar-close-tab)))))

(defun +tab-bar-tab-format-function (tab i)
  (let ((current-p (eq (car tab) 'current-tab)))
    (concat
     (propertize (concat
                  " "
                  (alist-get 'name tab)
                  " ")
                 'face
                 (funcall tab-bar-tab-face-function tab))
     " ")))

(global-set-key (kbd "C-x t .") #'tab-bar-rename-tab)
(global-set-key (kbd "C-x t l") #'+tab-bar-switch-project)

;; `gnus'
(setq
 gnus-use-cache t
 gnus-use-scoring nil
 gnus-suppress-duplicates t
 gnus-novice-user nil
 gnus-expert-user t
 gnus-interactive-exit 'quiet
 gnus-select-method '(nnnil "")
 gnus-secondary-select-methods '((nntp "gmane" (nntp-address "news.gmane.io"))
                                 (nntp "nntp.lore.kernel.org"))
    ;;; Startup functions
 gnus-save-killed-list nil
 gnus-check-new-newsgroups 'ask-server
 ;; No other newsreader is used.
 gnus-save-newsrc-file nil
 gnus-read-newsrc-file nil
 gnus-subscribe-newsgroup-method 'gnus-subscribe-interactively
 ;; Emacs 28 introduces a unified query lang
 gnus-search-use-parsed-queries t
    ;;; Article mode for Gnus
 gnus-visible-headers (rx line-start (or "From"
                                         "Subject"
                                         "Mail-Followup-To"
                                         "Date"
                                         "To"
                                         "Cc"
                                         "Newsgroups"
                                         "User-Agent"
                                         "X-Mailer"
                                         "X-Newsreader")
                          ":")
 gnus-article-sort-functions '((not gnus-article-sort-by-number)
                               (not gnus-article-sort-by-date))
 gnus-article-browse-delete-temp t
 ;; Display more MINE stuff
 gnus-mime-display-multipart-related-as-mixed t
  ;;; Asynchronous support for Gnus
 gnus-asynchronous t
 gnus-use-header-prefetch t
  ;;; Cache interface for Gnus
 gnus-cache-enter-articles '(ticked dormant unread)
 gnus-cache-remove-articles '(read)
 gnus-cacheable-groups "^\\(nntp\\|nnimap\\)")


;; `gnus-group'
(setq
 ;;          indentation ------------.
 ;;  #      process mark ----------. |
 ;;                level --------. | |
 ;;           subscribed ------. | | |
 ;;  %          new mail ----. | | | |
 ;;  *   marked articles --. | | | | |
 ;;                        | | | | | |  Ticked    New     Unread  open-status Group
 gnus-group-line-format "%M%m%S%L%p%P %1(%7i%) %3(%7U%) %3(%7y%) %4(%B%-45G%) %d\n"
 gnus-group-sort-function '(gnus-group-sort-by-level gnus-group-sort-by-alphabet))

(add-hook 'gnus-group-mode-hook #'gnus-topic-mode)

;; `gnus-sum'
(setq
 ;; Pretty marks
 gnus-sum-thread-tree-root            "┌ "
 gnus-sum-thread-tree-false-root      "◌ "
 gnus-sum-thread-tree-single-indent   "◎ "
 gnus-sum-thread-tree-vertical        "│"
 gnus-sum-thread-tree-indent          "  "
 gnus-sum-thread-tree-leaf-with-other "├─►"
 gnus-sum-thread-tree-single-leaf     "╰─►"
 gnus-summary-line-format "%U%R %3d %[%-23,23f%] %B %s\n"
 ;; Loose threads
 gnus-summary-make-false-root 'adopt
 gnus-simplify-subject-functions '(gnus-simplify-subject-re gnus-simplify-whitespace)
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
 ;; Filling in threads
 ;; 2 old articles are enough for memory
 gnus-fetch-old-headers 2
 gnus-fetch-old-ephemeral-headers 2
 gnus-build-sparse-threads 'some
 ;; More threading
 gnus-show-threads t
 gnus-thread-indent-level 2
 gnus-thread-hide-subtree nil
 ;; Sorting
 gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
 gnus-subthread-sort-functions '(gnus-thread-sort-by-date)
 ;; Viewing
 gnus-view-pseudos 'automatic
 gnus-view-pseudos-separately t
 gnus-view-pseudo-asynchronously t
 ;; No auto select
 gnus-auto-select-first nil
 gnus-auto-select-next nil)

(add-hook 'gnus-select-group-hook #'gnus-group-set-timestamp)

;; `flymake'
(autoload #'flymake "flymake" nil t)

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "C-c C-b") 'flymake-show-diagnostics-buffer))

;; `message'
(setq
 user-full-name "Liu Bo"
 user-mail-address "liubolovelife@gmail.com"
 message-kill-buffer-on-exit t
 message-mail-alias-type 'ecomplete
 message-send-mail-function #'message-use-send-mail-function
 message-signature user-full-name)

(add-hook 'message-mode-hook #'auto-fill-mode)

;; `sendmail'
(setq send-mail-function #'smtpmail-send-it)

;; `smtpmail'
(setq
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-smtp-user user-mail-address
 smtpmail-smtp-service 587
 smptmail-stream-type 'ssl)

;; `cc-mode'
(setq c-default-style "linux")
(setq-default c-basic-offset 4)

;; `python'
(setq
 python-indent-offset 4
 python-shell-completion-native-enable nil
 python-shell-interpreter "ipython"
 python-indent-guess-indent-offset nil)

;; `dired'
(setq
 dired-dwim-target t
 dired-kill-when-opening-new-dired-buffer t)

;; `modus-theme'
(setq modus-themes-mode-line '(accented barderless))

(provide 'init-builtin)
