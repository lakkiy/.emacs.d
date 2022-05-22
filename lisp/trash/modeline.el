(benchmark-run 1000 (format-mode-line mode-line-format))

(eat-package telephone-line
  :straight t
  :hook (after-init-hook . telephone-line-mode)
  :init
  (defvar modeline-height 17)
  ;; Set mode-line height
  (setq telephone-line-height modeline-height)

  (setq-default mode-line-format nil)
  (require 'telephone-segments)

  ;; Set separator style
  (setq telephone-line-primary-left-separator 'telephone-line-halfsin-left)
  (setq telephone-line-primary-right-separator 'telephone-line-halfsin-right)

  ;; Set subseparator
  (when window-system
    (setq telephone-line-secondary-left-separator 'telephone-line-identity-hollow-left
          telephone-line-secondary-right-separator 'telephone-line-identity-hollow-right))

  ;; Left edge
  ;; meow project-buffer
  (setq telephone-line-lhs
        '((accent . ((my-meow-segment :active)))
          (nil . (my-project-buffer-segment))
          (nil . (my-modified-status-segment))
          (nil . (my-read-only-status-segment))
          ))
  ;; (nil    . (my-flycheck-segment))))

  ;; Right edge
  ;; vc pos major encoding
  (setq telephone-line-rhs
        '((nil    . (my-rime-segment))
          (nil    . (my-vc-segment))
          (accent . ((my-position-segment :active)))
          (nil    . ((my-major-mode-segment-icon :active)))
          (accent . ((my-coding-segment :active)))
          (nil    . (my-flymake-segment))
          ))

  )

;;; -*- lexical-binding: t -*-

;; `telephone-line' left segments

;; meow segment
(telephone-line-defsegment my-meow-segment ()
  "Display meow state as text symbol."
  (when (telephone-line-selected-window-active)
    (let ((tag (cond
                ;; ((not (boundp 'meow))       "")
                ((string= meow--current-state "normal") ":")
                ((string= meow--current-state "insert") ">")
                ((string= meow--current-state "motion") "~")
                ((string= meow--current-state "beacon") "!")
                ((string= meow--current-state "keypad") "=")
                (t "-"))))
      (format " %s" tag))))
(telephone-line-defsegment my-meow-segment-icon ()
  "Display meow state as icon with all-the-icons."
  (let ((tag (cond
              ((string= meow--current-state "normal") (all-the-icons-faicon "magic"))
              ((string= meow--current-state "insert") (all-the-icons-faicon "pencil"))
              ((string= meow--current-state "motion") (all-the-icons-faicon "eraser"))
              ((string= meow--current-state "beacon") (all-the-icons-faicon "clipboard"))
              ((string= meow--current-state "keypad") (all-the-icons-faicon "angle-double-right"))
              (t "-"))))
    (format " %s" tag)))

(telephone-line-defsegment my--project-segment ()
  (propertize (+project-name)
              'face 'telephone-line-projectile
              'display '(raise 0.0)
              'help-echo "Switch project"
              'local-map (make-mode-line-mouse-map
                          'mouse-1 (lambda ()
                                     (interactive)
                                     (project-switch-project)))))

(telephone-line-defsegment* my-project-buffer-segment ()
  ""
  (if (and (buffer-file-name)
           (project-current))
      (list ""
            (funcall (my--project-segment) 'telephone-line-unimportant)
            (propertize
             (if-let ((rel-path (file-relative-name (file-truename (buffer-file-name))
                                                    (+project-name))))
                 (telephone-line--truncate-path rel-path -1)) ;; TODO need my own version
             'help-echo (buffer-file-name)))
    (telephone-line-raw mode-line-buffer-identification t)))

;; Exclude some buffers in modeline
(defvar modeline-ignored-modes '("Dashboard"
                                 "Warnings"
                                 "Compilation"
                                 "EShell" "Eshell"
                                 "Debugger"
                                 "Quickrun"
                                 "REPL"
                                 "IELM"
                                 "Messages"
                                 "Interactive-Haskell")
  "List of major modes to ignore in modeline")

;; Display modified status
(telephone-line-defsegment my-modified-status-segment ()
  (when (and (buffer-modified-p) (not (member mode-name modeline-ignored-modes)) (not buffer-read-only))
    (propertize "+" 'face `(:foreground "#85b654"))))

;; Display read-only status
(telephone-line-defsegment my-read-only-status-segment ()
  (when (and buffer-read-only (telephone-line-selected-window-active))
    ;; (propertize "ro" 'face `(:foreground "#dbac66"))
    (propertize (all-the-icons-octicon "key")
                'face `(:family ,(all-the-icons-octicon-family) :height 1.0 :foreground "dim gray")
                'display '(raise 0.0))))


;; `telephone-line' right segments

(telephone-line-defsegment my-rime-segment ()
                           (rime-lighter))

;; Display current branch
(telephone-line-defsegment my-vc-segment ()
                           (when (and vc-mode (telephone-line-selected-window-active))
                             ;; double format to prevent warnings in '*Messages*' buffer
                             (format "%s %s"
                                     (propertize (all-the-icons-octicon "git-branch")
                                                 'face `( :family ,(all-the-icons-octicon-family)
                                                          :height 1.0
                                                          :foreground ,(face-foreground 'font-lock-variable-name-face))
                                                 'display '(raise 0.0))
                                     (propertize
                                      (format "%s"
                                              (telephone-line-raw vc-mode t))
                                      'face `(:foreground ,(face-foreground 'font-lock-variable-name-face))))))

(telephone-line-defsegment my-position-segment (&optional lines columns)
                           "Position segment. Optional args set padding on lines/columns."
                           (when (telephone-line-selected-window-active)
                             (let* ((l (number-to-string (if lines lines 3)))
                                    (c (number-to-string (if columns columns 2))))
                               (if (eq major-mode 'paradox-menu-mode)
                                   (telephone-line-raw mode-line-front-space t)
                                 `(,(concat " %" l "l:%" c "c"))))))

;; Display major mode
(telephone-line-defsegment* my-major-mode-segment ()
                            (let* ((name (if (or (version< emacs-version "28.0") (stringp mode-name))
                                             mode-name
                                           (car mode-name)))
                                   (mode (cond
                                          ((string= name "Fundamental") "text")
                                          ((string= name "Emacs-Lisp") "elisp")
                                          ((string= name "Javascript-IDE") "js")
                                          ((string= name "undo-tree-visualizer") "undotree")
                                          ((string= name "C++//l") "cpp")
                                          (t (downcase name)))))
                              (propertize mode 'face `font-lock-string-face)))

(telephone-line-defsegment* my-major-mode-segment-icon ()
                            (let* ((name (if (or (version< emacs-version "28.0") (stringp mode-name))
                                             mode-name
                                           (car mode-name)))
                                   (mode (cond
                                          ((string= name "Fundamental") "text")
                                          ((string= name "Emacs-Lisp") "elisp")
                                          ((string= name "ELisp") "elisp")
                                          ((string= name "Javascript-IDE") "js")
                                          ((string= name "undo-tree-visualizer") "undotree")
                                          ((string= name "C++//l") "cpp")
                                          (t (downcase name))))
                                   (icon ( all-the-icons-icon-for-mode major-mode
                                           :v-adjust 0.0
                                           :height 0.8
                                           :face font-lock-string-face)))
                              (concat
                               (when
                                   (and (not (eq major-mode (all-the-icons-icon-for-mode major-mode)))
                                        (telephone-line-selected-window-active))
                                 (format "%s " icon))
                               (propertize mode 'face `font-lock-string-face))))

;; Display encoding system
(telephone-line-defsegment my-coding-segment ()
  (when (telephone-line-selected-window-active)
    (let* ((code (symbol-name buffer-file-coding-system))
           (eol-type (coding-system-eol-type buffer-file-coding-system))
           (eol (cond
                 ((eq 0 eol-type) "unix")
                 ((eq 1 eol-type) "dos")
                 ((eq 2 eol-type) "mac")
                 (t "-"))))
      (format  "%s " eol))))

(telephone-line-defsegment my-flymake-segment ()
                           (when (bound-and-true-p flymake-mode)
                             flymake-mode-line-format))


;;; `doom-modeline'
(eat-package doom-modeline
  :straight t
  :hook
  (after-init-hook . doom-modeline-mode)
  :init
  (setq doom-modeline-irc nil
        doom-modeline-mu4e nil
        doom-modeline-gnus nil
        doom-modeline-github nil
        doom-modeline-persp-name nil
        doom-modeline-unicode-fallback t
        doom-modeline-enable-work-count nil)
  (setq doom-modeline-project-detection 'project)
  :config
  (doom-modeline-def-modeline 'my
                              '(bar modals matches follow buffer-info remote-host buffer-position word-count selection-info)
                              '(objed-state misc-info battery debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker))
  (defun setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'my 'default))
  (add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline))

(eat-package awesome-tray
  :straight (awesome-tray :type git :host github :repo "manateelazycat/awesome-tray")
  :init
  (setq
   awesome-tray-update-interval 0.5
   awesome-tray-minibuffer nil
   awesome-tray-essential-modules nil
   awesome-tray-info-padding-right 2 ;; or it will warp by meow
   ;; TODO  belong not work
   awesome-tray-active-modules '("buffer-read-only" "buffer-name" "mode-name" "belong" "location"))

  :config
  ;;Make the modeline in GUI a thin bar.
  (defface mini-modeline-mode-line
    `((((background light))
       :background ,awesome-tray-mode-line-active-color :height 0.1 :box nil)
      (t
       :background ,awesome-tray-mode-line-active-color :height 0.1 :box nil))
    "Modeline face for active window.")

  (defface mini-modeline-mode-line-inactive
    `((((background light))
       :background ,awesome-tray-mode-line-inactive-color :height 0.1 :box nil)
      (t
       :background ,awesome-tray-mode-line-inactive-color :height 0.1 :box nil))
    "Modeline face for inactive window.")

  (setq-default mode-line-format (when (display-graphic-p)
                                   '(" ")))

  ;; Do the same thing with opening buffers.
  (mapc
   (lambda (buf)
     (with-current-buffer buf
       (when (local-variable-p 'mode-line-format)
         (setq mode-line-format (when (display-graphic-p)
                                  '(" "))))
       ;; Make the modeline in GUI a thin bar.
       (when (and (local-variable-p 'face-remapping-alist)
                  (display-graphic-p))
         (setf (alist-get 'mode-line face-remapping-alist)
               'mini-modeline-mode-line
               (alist-get 'mode-line-inactive face-remapping-alist)
               'mini-modeline-mode-line-inactive))))
   (buffer-list))

  ;; Make the modeline in GUI a thin bar.
  (when (and (display-graphic-p))
    (let ((face-remaps (default-value 'face-remapping-alist)))
      (setf (alist-get 'mode-line face-remaps)
            'mini-modeline-mode-line
            (alist-get 'mode-line-inactive face-remaps)
            'mini-modeline-mode-line-inactive
            (default-value 'face-remapping-alist) face-remaps)))


  ;; `popper', disable
  (with-eval-after-load 'popper
    (setq popper-mode-line " "))


  ;; `rime', add
  (with-eval-after-load 'rime
    ;; NOTE show in message have some error, wont on screen after choose
    (setq rime-show-candidate 'sidewindow
          rime-sidewindow-side 'top
          rime-sidewindow-keep-window t)

    (add-to-list 'awesome-tray-module-alist
                 '("rime" . (rime-lighter)))
    (add-to-list 'awesome-tray-active-modules "rime"))


  ;; `flymake', add
  (with-eval-after-load 'flymake
    (add-to-list 'awesome-tray-module-alist
                 '("flymake" . (sekiro-flymake-mode-line-format)))
    (add-to-list 'awesome-tray-active-modules "flymake"))


  ;; `meow', add
  (with-eval-after-load 'meow
    (defun awesome-tray-module-meow-info ()
      (string-trim (meow-indicator)))
    (add-to-list 'awesome-tray-module-alist
                 '("meow" . (awesome-tray-module-meow-info awesome-tray-module-evil-face)))
    (add-to-list 'awesome-tray-active-modules "meow"))


  ;; `eyebrowse', add
  (with-eval-after-load 'eyebrowse
    (add-to-list 'awesome-tray-module-alist
                 '("eyebrowse" . (eyebrowse-mode-line-indicator)))
    (add-to-list 'awesome-tray-active-modules "eyebrowse")))
