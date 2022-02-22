(benchmark-run 1000 (format-mode-line mode-line-format))

;; Hide vc backend in modeline
(defadvice vc-mode-line (after strip-backend () activate)
  (when (stringp vc-mode)
    (let ((my-vc (replace-regexp-in-string "^ Git." "" vc-mode)))
      (setq vc-mode my-vc))))

(defun +smart-file-name-cached ()
  (if (eq (buffer-name) (car +smart-file-name-cache))
      (cdr +smart-file-name-cache)
    (let ((file-name (+smart-file-name)))
      (setq +smart-file-name-cache
            (cons (buffer-name) file-name))
      file-name)))

(defvar +smart-file-name-cache nil)

(defun +smart-file-name ()
  "Get current file name, if we are in project, the return relative path to the project root, otherwise return absolute file path.
This function is slow, so we have to use cache."
  (let ((vc-dir (vc-root-dir))
        (bfn (buffer-file-name (current-buffer))))
    (cond
     ((and bfn vc-dir)
      (file-relative-name bfn vc-dir))
     (bfn bfn)
     (t (buffer-name)))))

(defun luna-mode-line-with-padding (text)
  "Return TEXT with padding on the left.
The padding pushes TEXT to the right edge of the mode-line."
  (if (display-graphic-p)
      (let* ((len (string-pixel-width text))
             (space-prop
              `(space :align-to (- (+ right right-margin) (,len))))
             (padding (propertize "-" 'display space-prop)))
        (concat padding text))
    (concat " " text)))

(setq-default mode-line-format
              (let* ((spaces
                      (propertize " " 'display '(space :width 1.5)))
                     (fringe (propertize
                              " " 'display '(space :width fringe)))
                     (percentage
                      '(format
                        "[%%l] %d%% "
                        (/ (* (window-end) 100.0) (point-max)))))
                `(,fringe
                  (:eval (when (fboundp 'meow-indicator) (meow-indicator)))
                  ,spaces
                  (:eval (when (fboundp 'rime-lighter) (rime-lighter)))
                  ,spaces
                  (:eval (propertize (+smart-file-name-cached) 'face 'mode-line-buffer-id))
                  ,spaces
                  ,spaces
                  mode-name
                  ,spaces
                  vc-mode
                  ,spaces
                  (:eval (when (bound-and-true-p flycheck-mode) flycheck-mode-line))
                  (:eval (when (bound-and-true-p flymake-mode) flymake-mode-line-format))
                  (:propertize " " display (height 1.4))
                  (:propertize " " display (raise -0.3))
                  (:eval (concat (luna-mode-line-with-padding ,percentage)
                                 "%%"))
                  )))

;; TODO process icon in terminal and check +icons-p
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
  ;; TODO: function to choose separator by name
  (when window-system
    (setq telephone-line-secondary-left-separator 'telephone-line-identity-hollow-left
          telephone-line-secondary-right-separator 'telephone-line-identity-hollow-right))

  ;; Left edge
  ;; meow project-buffer
  ;; TODO: gray background for buffer and mode segment in inactive line
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

  ;; TODO rime flymake/flycheck anzu
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
;; TODO: move raise and etc into var
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
;; TODO: rewrite with var/macro
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

;; TODO: add raise or v-adjust
;; TODO icon higher than
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
