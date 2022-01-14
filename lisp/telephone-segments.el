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

;; `telephone-line' right segments

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

(provide 'telephone-segments)
